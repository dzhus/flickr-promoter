# flickr-promoter

## Configuration

| Environment variable name      | Description                                                                  |
|--------------------------------|------------------------------------------------------------------------------|
| `FLICKR_PROMOTER_API_KEY`      | API key from `https://www.flickr.com/services/apps/by/...`                   |
| `FLICKR_PROMOTER_API_SECRET`   | App secret from the same page as above                                       |
| `FLICKR_PROMOTER_ACCESS_TOKEN` | Base64-encoded OAuth credential. If not set, authorisation will be performed |

## How to use

### Set up

1. Create a Flickr API app with "Web Application" app type using
`https://gist.github.com/dzhus/0bf2a8b1990c288315411ce69bca56df` as a
callback URL.

2. Set `FLICKR_PROMOTER_API_KEY` and `FLICKR_PROMOTER_API_SECRET` env
   variables using app keys.

3. Run `flickr-promoter`.

4. Complete authorisation and put the serialised token in
   `FLICKR_PROMOTER_ACCESS_TOKEN` variable as instructed.

### Normal mode

Run `flickr-promoter` with both env variables set.
