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
from flickr_promoter.flickr_client import FlickrClient
from flickr_promoter.rules import matching_groups
from flickr_promoter.throttle import Throttle
from flickr_promoter.types import GroupId, Photo

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
        return group_limits

    try:
        response = client.pools_add(photo.id, target_group)
    except Exception as exc:
        logger.error(
            "Unknown error posting %s to %s: %s",
            photo,
            target_group,
            exc,
        )
        return group_limits

    if response.stat == "ok":
        logger.info("Posted %s to %s", photo, target_group)
        updated = _started_posting() if target_group not in group_limits else _one_posted(
            group_limits[target_group]
        )
    elif response.stat == "fail":
        logger.warning(
            "Error posting %s to group %s: %s",
            photo,
            target_group,
            _format_pool_error(response.code),
        )
        if target_group in group_limits:
            updated = _none_left(group_limits[target_group])
        else:
            updated = _never_posted()
    else:
        logger.error(
            "Unknown error posting %s to %s: %s",
            photo,
            target_group,
            response,
        )
        return group_limits

    return {**group_limits, target_group: updated}


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

    photos: list[Photo] = []
    errors: list[BaseException] = []

    def gather_one(digest):
        return client.gather_photo_info(digest)

    with ThreadPoolExecutor(max_workers=GATHER_WORKERS) as executor:
        futures = {executor.submit(gather_one, d): d for d in digests}
        for future in as_completed(futures):
            try:
                photos.append(future.result())
            except Exception as exc:
                errors.append(exc)

    if errors:
        for exc in errors:
            logger.error("%s", exc)
        sys.exit(1)

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
    logger.info("Added %d new photos to groups", total_posted)

    depleted = [gid for gid, info in group_limits.items() if info.left == 0]
    if depleted:
        logger.info(
            "Posting limits reached for %d groups: %s",
            len(depleted),
            depleted,
        )


def main() -> None:
    logging.basicConfig(
        level=logging.INFO,
        format="%(levelname)s: %(message)s",
    )
    process(_parse_args())


if __name__ == "__main__":
    main()
