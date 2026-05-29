from __future__ import annotations

from collections.abc import Callable
from dataclasses import dataclass

from flickr_promoter.types import GroupId, Photo, Tag


@dataclass(frozen=True)
class Rule:
    predicate: Callable[[Photo], bool]
    group_id: GroupId


def rule(predicate: Callable[[Photo], bool], group_id: str) -> Rule:
    return Rule(predicate, GroupId(group_id))


def any_(photo: Photo) -> bool:
    return True


def located_in(text: str) -> Callable[[Photo], bool]:
    def check(photo: Photo) -> bool:
        if photo.location is None:
            return False
        return text in photo.location.text

    return check


def has_tag(tag: str) -> Callable[[Photo], bool]:
    t = Tag(tag)

    def check(photo: Photo) -> bool:
        return t in photo.tags

    return check


def more_faves_than(threshold: int) -> Callable[[Photo], bool]:
    def check(photo: Photo) -> bool:
        return photo.faves >= threshold

    return check


def and_(r1: Callable[[Photo], bool], r2: Callable[[Photo], bool]) -> Callable[[Photo], bool]:
    def check(photo: Photo) -> bool:
        return r1(photo) and r2(photo)

    return check


rules: list[Rule] = [
    rule(more_faves_than(1), "1755214@N23"),
    rule(more_faves_than(1), "1136489@N22"),
    rule(more_faves_than(1), "1902869@N24"),
    rule(more_faves_than(1), "34427469792@N01"),
    rule(more_faves_than(1), "3537491@N25"),
    rule(more_faves_than(1), "40732569271@N01"),
    rule(more_faves_than(1), "416556@N22"),
    rule(more_faves_than(1), "76535076@N00"),
    rule(more_faves_than(1), "11252682@N00"),
    rule(more_faves_than(1), "28747776@N00"),
    rule(more_faves_than(1), "91514935@N00"),
    rule(more_faves_than(1), "52240402017@N01"),
    rule(more_faves_than(1), "14805334@N23"),
    rule(more_faves_than(1), "68567710@N00"),
    rule(more_faves_than(1), "58286952@N00"),
    rule(more_faves_than(1), "20759249@N00"),
    rule(more_faves_than(1), "2161940@N25"),
    rule(any_, "557255@N22"),
    rule(any_, "95309787@N00"),
    rule(any_, "1148171@N20"),
    rule(any_, "58898522@N00"),
    rule(any_, "38436807@N00"),
    rule(any_, "2677807@N23"),
    rule(any_, "43501458@N00"),
    rule(any_, "769299@N22"),
    rule(any_, "2978869@N23"),
    rule(located_in("Bavaria"), "860590@N23"),
    rule(located_in("Crimea"), "60453939@N00"),
    rule(and_(has_tag("landscape"), located_in("Cumbria")), "53837206@N00"),
    rule(located_in("England"), "35468144964@N01"),
    rule(located_in("France"), "52241533836@N01"),
    rule(located_in("London"), "2625353@N20"),
    rule(located_in("Lyon"), "13409106@N00"),
    rule(located_in("Paris"), "36101698174@N01"),
    rule(located_in("Prague"), "48889111127@N01"),
    rule(located_in("Italy"), "31746602@N00"),
    rule(located_in("Italy"), "37996580003@N01"),
    rule(located_in("Rome"), "59943000@N00"),
    rule(located_in("Russia"), "288127@N25"),
    rule(located_in("Scotland"), "37887068055@N01"),
    rule(and_(has_tag("landscape"), located_in("Scotland")), "70163666@N00"),
    rule(located_in("Switzerland"), "41894179852@N01"),
    rule(located_in("Switzerland"), "67376880@N00"),
    rule(has_tag("landscape"), "13197975@N00"),
    rule(has_tag("landscape"), "650323@N24"),
    rule(has_tag("landscape"), "2241717@N21"),
    rule(has_tag("landscape"), "23854677@N00"),
    rule(has_tag("nature"), "81431815@N00"),
    rule(has_tag("landscape"), "80148101@N00"),
    rule(has_tag("landscape"), "11611663@N00"),
    rule(has_tag("landscape"), "535727@N21"),
    rule(has_tag("landscape"), "1003995@N21"),
]


def matching_groups(photo: Photo) -> set[GroupId]:
    return {
        r.group_id
        for r in rules
        if r.predicate(photo) and r.group_id not in photo.groups
    }
