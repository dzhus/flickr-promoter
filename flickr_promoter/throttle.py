from __future__ import annotations

import threading
import time
from collections import deque
from collections.abc import Callable
from typing import TypeVar

T = TypeVar("T")

THROTTLING_INTERVAL_SECONDS = 5.0
REQUESTS_PER_UNIT = 1


class Throttle:
    """At most one Flickr API call every THROTTLING_INTERVAL_SECONDS."""

    def __init__(
        self,
        capacity: int = REQUESTS_PER_UNIT,
        interval_seconds: float = THROTTLING_INTERVAL_SECONDS,
    ) -> None:
        self._capacity = capacity
        self._interval = interval_seconds
        self._lock = threading.Lock()
        self._timestamps: deque[float] = deque(maxlen=capacity)

    def run(self, action: Callable[[], T]) -> T:
        while True:
            delay = self._acquire_slot()
            if delay is None:
                return action()
            time.sleep(delay)

    def _acquire_slot(self) -> float | None:
        with self._lock:
            now = time.monotonic()
            if len(self._timestamps) < self._capacity:
                self._timestamps.append(now)
                return None

            oldest = self._timestamps[0]
            elapsed = now - oldest
            if elapsed >= self._interval:
                self._timestamps.popleft()
                self._timestamps.append(now)
                return None

            return self._interval - elapsed
