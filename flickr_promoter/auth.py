from __future__ import annotations

import os
from pathlib import Path
from urllib.parse import parse_qs, urlparse

import flickr_api
from flickr_api.auth import AuthHandler

OAUTH_CALLBACK = "https://gist.github.com/dzhus/0bf2a8b1990c288315411ce69bca56df"
DEFAULT_AUTH_FILE = Path.home() / ".config" / "flickr-promoter" / "auth"


def default_auth_file() -> Path:
    configured = os.environ.get("FLICKR_PROMOTER_AUTH_FILE")
    if configured is not None:
        return Path(configured).expanduser()
    return DEFAULT_AUTH_FILE


def extract_oauth_verifier(url_or_code: str) -> str:
    text = url_or_code.strip()
    if "oauth_verifier=" not in text and "://" not in text:
        return text
    parsed = urlparse(text)
    params = parse_qs(parsed.query)
    verifier = params.get("oauth_verifier", [None])[0]
    if not verifier:
        raise ValueError(
            "No oauth_verifier parameter found in the URL copied. "
            "Make sure you copy it correctly."
        )
    return verifier


def configure_api_keys(api_key: str, api_secret: str) -> None:
    flickr_api.set_keys(api_key=api_key, api_secret=api_secret)


def load_auth_handler(
    api_key: str,
    api_secret: str,
    auth_file: Path | None = None,
) -> AuthHandler | None:
    oauth_token = os.environ.get("FLICKR_PROMOTER_OAUTH_TOKEN")
    oauth_secret = os.environ.get("FLICKR_PROMOTER_OAUTH_TOKEN_SECRET")
    if oauth_token and oauth_secret:
        return AuthHandler(
            key=api_key,
            secret=api_secret,
            access_token_key=oauth_token,
            access_token_secret=oauth_secret,
        )

    path = auth_file or default_auth_file()
    if path.is_file():
        return AuthHandler.load(str(path), set_api_keys=True)

    return None


def run_interactive_auth(api_key: str, api_secret: str, auth_file: Path) -> AuthHandler:
    handler = AuthHandler(key=api_key, secret=api_secret, callback=OAUTH_CALLBACK)
    url = handler.get_authorization_url("write")
    print(
        "To authorize flickr-promoter, open the following URL:\n"
        f"{url}\n\n"
        "When you complete authorisation, copy the URL from the address bar here:"
    )
    verifier = extract_oauth_verifier(input())
    handler.set_verifier(verifier)
    auth_file.parent.mkdir(parents=True, exist_ok=True)
    handler.save(str(auth_file))
    return handler


def setup_session(
    api_key: str,
    api_secret: str,
    auth_file: Path | None = None,
) -> AuthHandler:
    configure_api_keys(api_key, api_secret)
    path = auth_file or default_auth_file()
    handler = load_auth_handler(api_key, api_secret, path)
    if handler is None:
        handler = run_interactive_auth(api_key, api_secret, path)
        print(f"Saved credentials to {path}")
        print(
            "For CI, set FLICKR_PROMOTER_OAUTH_TOKEN and "
            "FLICKR_PROMOTER_OAUTH_TOKEN_SECRET from the auth file lines."
        )
        raise SystemExit(1)
    flickr_api.set_auth_handler(handler)
    return handler
