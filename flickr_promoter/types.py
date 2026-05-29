from __future__ import annotations

from dataclasses import dataclass, field
from typing import NewType

PhotoId = NewType("PhotoId", str)
GroupId = NewType("GroupId", str)
Tag = NewType("Tag", str)


@dataclass
class Location:
    text: str


@dataclass
class Photo:
    id: PhotoId
    title: str
    tags: set[Tag] = field(default_factory=set)
    groups: set[GroupId] = field(default_factory=set)
    location: Location | None = None
    views: int = 0
    faves: int = 0

    def __str__(self) -> str:
        return f"Photo({self.id!s}, {self.title!r})"
