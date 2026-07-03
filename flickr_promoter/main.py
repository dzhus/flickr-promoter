from __future__ import annotations

import argparse
import csv
import logging
import os
import random
import sys
from concurrent.futures import ThreadPoolExecutor, as_completed
from dataclasses import dataclass
from pathlib import Path

from flickr_promoter.auth import setup_session
from flickr_promoter.flickr_client import FlickrClient, PhotoDigest
from flickr_promoter.log_safety import configure_logging, safe_exception_summary
from flickr_promoter.metadata_cache import (
    default_metadata_cache_file,
    load as load_metadata_cache,
    photo_from_cache_with_digest,
    save as save_metadata_cache,
)
from flickr_promoter.rules import matching_groups
from flickr_promoter.throttle import Throttle
from flickr_promoter.types import GroupId, Photo, PhotoId

PHOTOS_PER_GROUP = 5
GATHER_WORKERS = 100

logger = logging.getLogger(__name__)


@dataclass
class GroupInfo:
    left: int
    posted: int


def _started_posting() -> GroupInfo:
    return GroupInfo(left=PHOTOS_PER_GROUP - 1, posted=1)


def _one_posted(info: GroupInfo) -> GroupInfo:
    return GroupInfo(left=info.left - 1, posted=info.posted + 1)


def _never_posted() -> GroupInfo:
    return GroupInfo(left=0, posted=0)


def _none_left(info: GroupInfo) -> GroupInfo:
    return GroupInfo(left=0, posted=info.posted)


def _disable_group(
    group_limits: dict[GroupId, GroupInfo],
    group_id: GroupId,
) -> dict[GroupId, GroupInfo]:
    existing = group_limits.get(group_id)
    if existing is not None:
        updated = _none_left(existing)
    else:
        updated = _never_posted()
    return {**group_limits, group_id: updated}


def _parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(prog="flickr-promoter")
    parser.add_argument(
        "-r",
        "--report",
        metavar="PATH",
        help="Path to CSV file to write photo views/faves statistics to",
    )
    parser.add_argument(
        "-d",
        "--no-posting",
        action="store_true",
        help="Do not actually post photos to any groups",
    )
    parser.add_argument(
        "-v",
        "--verbose",
        action="store_true",
        help="Enable debug logging (per-photo API steps; no secrets or tracebacks)",
    )
    return parser.parse_args()


def _write_report(path: Path, photos: list[Photo]) -> None:
    with path.open("w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=["id", "title", "views", "faves"])
        writer.writeheader()
        for photo in photos:
            writer.writerow(
                {
                    "id": str(photo.id),
                    "title": photo.title,
                    "views": photo.views,
                    "faves": photo.faves,
                }
            )


def _format_pool_error(code: int | None) -> str:
    if code == 5:
        return "GroupLimit"
    if code == 2:
        return "GroupNotFound"
    if code == 8:
        return "InappropriateContent"
    return f"UnknownCode({code})"


def _post_to_group(
    client: FlickrClient,
    photo: Photo,
    group_limits: dict[GroupId, GroupInfo],
    target_group: GroupId,
) -> dict[GroupId, GroupInfo]:
    info = group_limits.get(target_group)
    if info is not None and info.left == 0:
        logger.debug("Skipping disabled group %s", target_group)
        return group_limits

    try:
        response = client.pools_add(photo.id, target_group)
    except Exception as exc:
        summary = safe_exception_summary(exc)
        logger.error(
            "Unknown error posting %s to %s: %s",
            photo,
            target_group,
            summary,
        )
        logger.info(
            "Disabled group %s for remainder of run (%s)",
            target_group,
            summary,
        )
        return _disable_group(group_limits, target_group)

    if response.stat == "ok":
        logger.info("Posted %s to %s", photo, target_group)
        updated = _started_posting() if target_group not in group_limits else _one_posted(
            group_limits[target_group]
        )
        return {**group_limits, target_group: updated}

    if response.stat == "fail":
        reason = _format_pool_error(response.code)
        logger.warning(
            "Error posting %s to group %s: %s",
            photo,
            target_group,
            reason,
        )
        logger.info(
            "Disabled group %s for remainder of run (%s)",
            target_group,
            reason,
        )
        return _disable_group(group_limits, target_group)

    logger.error(
        "Unknown error posting %s to %s: %s",
        photo,
        target_group,
        response,
    )
    logger.info(
        "Disabled group %s for remainder of run (unexpected response)",
        target_group,
    )
    return _disable_group(group_limits, target_group)


def _process_photo(
    client: FlickrClient,
    group_limits: dict[GroupId, GroupInfo],
    photo: Photo,
) -> dict[GroupId, GroupInfo]:
    groups = matching_groups(photo)
    if not groups:
        return group_limits

    logger.debug(
        "%s/%s should be in groups: %s",
        photo.title,
        photo.id,
        ", ".join(str(g) for g in groups),
    )

    for group_id in groups:
        group_limits = _post_to_group(client, photo, group_limits, group_id)
    return group_limits


def _gather_photos_from_digests(
    client: FlickrClient,
    digests: list[PhotoDigest],
) -> tuple[list[Photo], list[tuple[PhotoId, BaseException]]]:
    photos: list[Photo] = []
    errors: list[tuple[PhotoId, BaseException]] = []
    completed = 0

    def gather_one(digest: PhotoDigest) -> Photo:
        return client.gather_photo_info(digest)

    workers = min(GATHER_WORKERS, max(len(digests), 1))
    with ThreadPoolExecutor(max_workers=workers) as executor:
        futures = {executor.submit(gather_one, d): d for d in digests}
        for future in as_completed(futures):
            digest = futures[future]
            completed += 1
            try:
                photos.append(future.result())
                logger.info(
                    "Gathered %d/%d: %s (%s)",
                    completed,
                    len(digests),
                    digest.id,
                    digest.title,
                )
            except Exception as exc:
                errors.append((digest.id, exc))
                summary = safe_exception_summary(exc)
                logger.error(
                    "Failed %d/%d: %s (%s): %s",
                    completed,
                    len(digests),
                    digest.id,
                    digest.title,
                    summary,
                )

    return photos, errors


def process(args: argparse.Namespace) -> None:
    api_key = os.environ.get("FLICKR_PROMOTER_API_KEY")
    api_secret = os.environ.get("FLICKR_PROMOTER_API_SECRET")
    if not api_key or not api_secret:
        sys.exit(
            "Populate FLICKR_PROMOTER_API_KEY and FLICKR_PROMOTER_API_SECRET "
            "from https://www.flickr.com/services/apps/by/..."
        )

    auth_handler = setup_session(api_key, api_secret)
    throttle = Throttle()
    client = FlickrClient(auth_handler, throttle)

    client.test_login()
    logger.info("Logged in to Flickr")

    digests = client.get_latest_photos()
    logger.info("Fetched %d latest photos", len(digests))

    cache_path = default_metadata_cache_file()
    cached = load_metadata_cache(cache_path) if cache_path.is_file() else {}
    if cached:
        logger.info("Loaded metadata cache from %s (%d entries)", cache_path, len(cached))

    missing_digests = [digest for digest in digests if digest.id not in cached]
    if missing_digests:
        logger.info(
            "Fetching metadata for %d/%d photos (cache miss)",
            len(missing_digests),
            len(digests),
        )

    photos: list[Photo] = [
        photo_from_cache_with_digest(
            cached[digest.id],
            title=digest.title,
            views=digest.views,
        )
        for digest in digests
        if digest.id in cached
    ]

    fetched_photos, errors = _gather_photos_from_digests(client, missing_digests)
    photos.extend(fetched_photos)

    if fetched_photos:
        for photo in fetched_photos:
            cached[photo.id] = photo
        save_metadata_cache(cache_path, cached)
        logger.info("Wrote metadata cache to %s (%d entries)", cache_path, len(cached))

    if errors:
        logger.error(
            "Metadata gather failed for %d of %d photos",
            len(errors),
            len(missing_digests),
        )
        for photo_id, exc in errors:
            logger.error("  %s: %s", photo_id, safe_exception_summary(exc))

    random.shuffle(photos)
    logger.info("Gathered details for %d photos", len(photos))

    if args.report:
        report_path = Path(args.report)
        _write_report(report_path, photos)
        logger.info("Wrote photo stats report to %s", report_path)

    group_limits: dict[GroupId, GroupInfo] = {}
    if not args.no_posting:
        for photo in photos:
            group_limits = _process_photo(client, group_limits, photo)

    total_posted = sum(info.posted for info in group_limits.values())
    logger.info("Made %d new photo postings", total_posted)

    depleted = [gid for gid, info in group_limits.items() if info.left == 0]
    if depleted:
        logger.info(
            "Posting limits reached for %d groups: %s",
            len(depleted),
            depleted,
        )


def main() -> None:
    args = _parse_args()
    configure_logging(verbose=args.verbose)
    process(args)


if __name__ == "__main__":
    main()
