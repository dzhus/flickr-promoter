from __future__ import annotations

import logging
from dataclasses import dataclass
from typing import Any

import flickr_api
from flickr_api import method_call
from flickr_api import objects as flickr_objects

from flickr_promoter.throttle import Throttle
from flickr_promoter.types import GroupId, Location, Photo, PhotoId, Tag

logger = logging.getLogger(__name__)

MAX_PHOTO_COUNT = 2000
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


def _extract_location(photo_info: dict[str, Any]) -> Location | None:
    loc = photo_info.get("location")
    if not loc or not isinstance(loc, dict):
        return None
    parts = [
        _content(loc.get("country")),
        _content(loc.get("region")),
        _content(loc.get("county")),
        _content(loc.get("locality")),
    ]
    text = ", ".join(p for p in parts if p)
    return Location(text) if text else None


def _extract_tags(photo_info: dict[str, Any]) -> set[Tag]:
    tags = photo_info.get("tags", [])
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
    def __init__(self, throttle: Throttle | None = None) -> None:
        self._throttle = throttle or Throttle()

    def _call(self, **kwargs: Any) -> dict[str, Any]:
        def do_call() -> dict[str, Any]:
            return method_call.call_api(**kwargs)

        return self._throttle.run(do_call)

    def test_login(self) -> None:
        self._call(method="flickr.test.login", needssigning=True)

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
        photo = flickr_objects.Photo(id=str(digest.id))
        info = self._throttle.run(photo.getInfo)
        _, pools = self._throttle.run(photo.getAllContexts)
        faves_list = self._throttle.run(photo.getFavorites)

        groups = {GroupId(str(pool.id)) for pool in pools}
        faves = int(getattr(faves_list.info, "total", 0) or 0)

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
            needssigning=True,
        )
        stat = str(response.get("stat", ""))
        code = response.get("code")
        parsed_code = int(code) if code is not None else None
        return PoolsAddResult(stat=stat, code=parsed_code)
