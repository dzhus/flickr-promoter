# flickr-promoter

This product uses the Flickr API but is not endorsed or certified by SmugMug, Inc.

## Configuration

| Environment variable name | Description |
|---------------------------|-------------|
| `FLICKR_PROMOTER_API_KEY` | API key from `https://www.flickr.com/services/apps/by/...` |
| `FLICKR_PROMOTER_API_SECRET` | App secret from the same page as above |
| `FLICKR_PROMOTER_AUTH_FILE` | Optional path to OAuth token file (default: `~/.config/flickr-promoter/auth`) |
| `FLICKR_PROMOTER_OAUTH_TOKEN` | OAuth access token (for CI; use with secret below) |
| `FLICKR_PROMOTER_OAUTH_TOKEN_SECRET` | OAuth access token secret |

After initial authorization, either set `FLICKR_PROMOTER_AUTH_FILE` to a saved auth file, or set both `FLICKR_PROMOTER_OAUTH_TOKEN` and `FLICKR_PROMOTER_OAUTH_TOKEN_SECRET`.

## How to use

### Set up

1. Create a Flickr API app with "Web Application" app type using
   `https://gist.github.com/dzhus/0bf2a8b1990c288315411ce69bca56df` as a
   callback URL.

2. Set `FLICKR_PROMOTER_API_KEY` and `FLICKR_PROMOTER_API_SECRET` env
   variables using app keys.

3. Install: `pip install .` (or `pip install -e .` for development).

4. Run `flickr-promoter`.

5. Complete authorisation; credentials are saved to the auth file (or use the
   printed instructions for CI secrets).

### Normal mode

Run `flickr-promoter` with API keys and OAuth credentials configured.

Options:

- `-r` / `--report PATH` — write CSV with photo id, title, views, and faves
- `-d` / `--no-posting` — fetch and evaluate rules without posting to groups
