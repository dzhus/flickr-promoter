from __future__ import annotations

import logging
from dataclasses import dataclass
from typing import Any

import flickr_api
from flickr_api import method_call
from flickr_api import objects as flickr_objects
from flickr_api.auth import AuthHandler

from flickr_promoter.throttle import Throttle
from flickr_promoter.types import GroupId, Location, Photo, PhotoId, Tag

logger = logging.getLogger(__name__)

MAX_PHOTO_COUNT = 3500
PER_PAGE = 500


@dataclass(frozen=True)
class PhotoDigest:
    id: PhotoId
    title: str
    views: int
    media: str


@dataclass(frozen=True)
class PoolsAddResult:
    stat: str
    code: int | None = None


def _content(value: Any) -> str:
    if value is None:
        return ""
    if isinstance(value, str):
        return value
    if isinstance(value, dict):
        return str(value.get("_content", value.get("content", "")))
    raw = getattr(value, "raw", None)
    if raw is not None:
        return str(raw)
    return str(value)


def _photo_field(photo_info: Any, key: str, default: Any = None) -> Any:
    if isinstance(photo_info, dict):
        return photo_info.get(key, default)
    return getattr(photo_info, key, default)


def _extract_location(photo_info: Any) -> Location | None:
    loc = _photo_field(photo_info, "location")
    if not loc:
        return None
    if isinstance(loc, dict):
        parts = [
            _content(loc.get("country")),
            _content(loc.get("region")),
            _content(loc.get("county")),
            _content(loc.get("locality")),
        ]
    else:
        parts = [
            _content(getattr(loc, "country", None)),
            _content(getattr(loc, "region", None)),
            _content(getattr(loc, "county", None)),
            _content(getattr(loc, "locality", None)),
        ]
    text = ", ".join(p for p in parts if p)
    return Location(text) if text else None


def _extract_tags(photo_info: Any) -> set[Tag]:
    tags = _photo_field(photo_info, "tags", [])
    if not isinstance(tags, list):
        return set()
    result: set[Tag] = set()
    for tag in tags:
        if isinstance(tag, str):
            result.add(Tag(tag))
        else:
            result.add(Tag(_content(tag) or getattr(tag, "raw", str(tag))))
    return result


def _parse_views(value: Any) -> int:
    if value is None:
        return 0
    if isinstance(value, int):
        return value
    return int(str(value))


class FlickrClient:
    def __init__(self, auth_handler: AuthHandler, throttle: Throttle | None = None) -> None:
        self._auth_handler = auth_handler
        self._throttle = throttle or Throttle()

    def _call(self, **kwargs: Any) -> dict[str, Any]:
        kwargs.setdefault("auth_handler", self._auth_handler)

        def do_call() -> dict[str, Any]:
            logger.debug("API call: %s", kwargs.get("method"))
            return method_call.call_api(**kwargs)

        return self._throttle.run(do_call)

    def test_login(self) -> None:
        self._call(method="flickr.test.login")

    def get_latest_photos(self, max_photos: int = MAX_PHOTO_COUNT) -> list[PhotoDigest]:
        person = flickr_objects.Person(id="me")
        page = 1
        collected: list[PhotoDigest] = []

        while len(collected) < max_photos:
            photo_list = self._throttle.run(
                lambda p=page: person.getPhotos(
                    extras="views,description,media",
                    content_types=0,
                    safe_search=1,
                    privacy_filter=1,
                    per_page=PER_PAGE,
                    page=p,
                )
            )
            for photo in photo_list:
                media = getattr(photo, "media", "photo")
                if str(media) != "photo":
                    continue
                collected.append(
                    PhotoDigest(
                        id=PhotoId(str(photo.id)),
                        title=str(getattr(photo, "title", "") or ""),
                        views=_parse_views(getattr(photo, "views", 0)),
                        media=str(media),
                    )
                )
                if len(collected) >= max_photos:
                    break

            info = photo_list.info
            if info.page >= info.pages:
                break
            page += 1

        return collected[:max_photos]

    def gather_photo_info(self, digest: PhotoDigest) -> Photo:
        photo_id = str(digest.id)
        logger.debug("Gathering info for %s (%r)", photo_id, digest.title)
        photo = flickr_objects.Photo(id=photo_id)

        logger.debug("  %s: photos.getInfo", photo_id)
        info = self._throttle.run(photo.getInfo)
        logger.debug("  %s: getInfo returned %s", photo_id, type(info).__name__)

        logger.debug("  %s: photos.getAllContexts", photo_id)
        _, pools = self._throttle.run(photo.getAllContexts)
        logger.debug("  %s: %d pool(s)", photo_id, len(pools))

        logger.debug("  %s: photos.getFavorites", photo_id)
        faves_list = self._throttle.run(photo.getFavorites)
        faves = int(getattr(faves_list.info, "total", 0) or 0)
        if faves == 0 and hasattr(faves_list, "__len__"):
            faves = len(faves_list)
        logger.debug("  %s: %d fave(s)", photo_id, faves)

        groups = {GroupId(str(pool.id)) for pool in pools}

        return Photo(
            id=digest.id,
            title=digest.title,
            tags=_extract_tags(info),
            groups=groups,
            location=_extract_location(info),
            faves=faves,
            views=digest.views,
        )

    def pools_add(self, photo_id: PhotoId, group_id: GroupId) -> PoolsAddResult:
        response = self._call(
            method="flickr.groups.pools.add",
            photo_id=str(photo_id),
            group_id=str(group_id),
        )
        stat = str(response.get("stat", ""))
        code = response.get("code")
        parsed_code = int(code) if code is not None else None
        return PoolsAddResult(stat=stat, code=parsed_code)
