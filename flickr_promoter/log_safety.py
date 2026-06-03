from __future__ import annotations

import logging
import re

from flickr_api.flickrerrors import FlickrAPIError, FlickrServerError

# Query-string style secrets that must never appear in logs.
_REDACT_PATTERNS: tuple[tuple[re.Pattern[str], str], ...] = (
    (re.compile(r"(oauth_verifier=)[^&\s\"']+", re.I), r"\1***"),
    (re.compile(r"(oauth_token=)[^&\s\"']+", re.I), r"\1***"),
    (re.compile(r"(oauth_token_secret=)[^&\s\"']+", re.I), r"\1***"),
    (re.compile(r"(oauth_signature=)[^&\s\"']+", re.I), r"\1***"),
    (re.compile(r"(api_secret=)[^&\s\"']+", re.I), r"\1***"),
    (re.compile(r"(api_key=)[^&\s\"']+", re.I), r"\1***"),
    (re.compile(r"(oauth_consumer_key=)[^&\s\"']+", re.I), r"\1***"),
)

# Loggers that may emit signed request parameters at DEBUG.
_QUIET_LOGGERS = (
    "flickr_api",
    "urllib3",
    "requests",
    "requests_oauthlib",
    "oauthlib",
)


class RedactingFilter(logging.Filter):
    """Last-resort scrub of OAuth/API material from log records."""

    def filter(self, record: logging.LogRecord) -> bool:
        if isinstance(record.msg, str):
            record.msg = redact_secrets(record.msg)
        if record.args:
            record.args = tuple(
                redact_secrets(arg) if isinstance(arg, str) else arg
                for arg in record.args
            )
        return True


def redact_secrets(text: str) -> str:
    redacted = text
    for pattern, replacement in _REDACT_PATTERNS:
        redacted = pattern.sub(replacement, redacted)
    return redacted


def safe_exception_summary(exc: BaseException) -> str:
    """Return a log-safe description of an exception (no tokens or response bodies)."""
    if isinstance(exc, FlickrAPIError):
        return f"FlickrAPIError(code={exc.code})"
    if isinstance(exc, FlickrServerError):
        return f"FlickrServerError(status={exc.status_code})"
    return type(exc).__name__


def configure_logging(*, verbose: bool) -> None:
    logging.basicConfig(
        level=logging.DEBUG if verbose else logging.INFO,
        format="%(levelname)s: %(message)s",
    )
    redacting = RedactingFilter()
    logging.getLogger().addFilter(redacting)
    for handler in logging.getLogger().handlers:
        handler.addFilter(redacting)

    logging.getLogger("flickr_promoter").setLevel(logging.DEBUG if verbose else logging.INFO)
    for name in _QUIET_LOGGERS:
        logging.getLogger(name).setLevel(logging.WARNING)
