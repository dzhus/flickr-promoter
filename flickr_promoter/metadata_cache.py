from __future__ import annotations

import json
import logging
import os
import tempfile
from pathlib import Path

from flickr_promoter.types import GroupId, Location, Photo, PhotoId, Tag

logger = logging.getLogger(__name__)

DEFAULT_METADATA_CACHE_FILE = (
    Path.home() / ".config" / "flickr-promoter" / "metadata-cache.json"
)


def default_metadata_cache_file() -> Path:
    configured = os.environ.get("FLICKR_PROMOTER_METADATA_CACHE_FILE")
    if configured is not None:
        return Path(configured).expanduser()
    return DEFAULT_METADATA_CACHE_FILE


def photo_to_dict(photo: Photo) -> dict:
    return {
        "id": str(photo.id),
        "title": photo.title,
        "tags": sorted(str(tag) for tag in photo.tags),
        "groups": sorted(str(group) for group in photo.groups),
        "location": photo.location.text if photo.location else None,
        "faves": photo.faves,
        "views": photo.views,
    }


def photo_from_dict(data: dict) -> Photo:
    location_text = data.get("location")
    return Photo(
        id=PhotoId(str(data["id"])),
        title=str(data.get("title", "")),
        tags={Tag(str(tag)) for tag in data.get("tags", [])},
        groups={GroupId(str(group)) for group in data.get("groups", [])},
        location=Location(location_text) if location_text else None,
        faves=int(data.get("faves", 0)),
        views=int(data.get("views", 0)),
    )


def photo_from_cache_with_digest(cached: Photo, *, title: str, views: int) -> Photo:
    return Photo(
        id=cached.id,
        title=title,
        tags=cached.tags,
        groups=cached.groups,
        location=cached.location,
        faves=cached.faves,
        views=views,
    )


def load(path: Path) -> dict[PhotoId, Photo]:
    if not path.is_file():
        return {}

    try:
        raw = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as exc:
        logger.error("Failed to read metadata cache from %s: %s", path, exc)
        return {}

    if not isinstance(raw, dict):
        logger.error("Invalid metadata cache format in %s: expected object", path)
        return {}

    photos: dict[PhotoId, Photo] = {}
    for photo_id, entry in raw.items():
        if not isinstance(entry, dict):
            logger.warning("Skipping invalid cache entry for %s", photo_id)
            continue
        try:
            photo = photo_from_dict(entry)
        except (KeyError, TypeError, ValueError) as exc:
            logger.warning("Skipping invalid cache entry for %s: %s", photo_id, exc)
            continue
        photos[photo.id] = photo

    return photos


def save(path: Path, photos: dict[PhotoId, Photo]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    payload = {str(photo_id): photo_to_dict(photo) for photo_id, photo in photos.items()}

    with tempfile.NamedTemporaryFile(
        mode="w",
        encoding="utf-8",
        dir=path.parent,
        delete=False,
        suffix=".tmp",
    ) as tmp:
        json.dump(payload, tmp, indent=2, sort_keys=True)
        tmp.write("\n")
        tmp_path = Path(tmp.name)

    tmp_path.replace(path)
