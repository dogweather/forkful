---
date: 2024-01-20 18:01:36.556352-07:00
description: "How to: You\u2019ll need the `http-conduit` package for HTTP actions\
  \ and `base64-bytestring` for encoding credentials. Import them and use `applyBasicAuth`\
  \ to\u2026"
lastmod: '2024-03-13T22:45:00.128155-06:00'
model: gpt-4-1106-preview
summary: "You\u2019ll need the `http-conduit` package for HTTP actions and `base64-bytestring`\
  \ for encoding credentials."
title: Sending an HTTP request with basic authentication
weight: 45
---

## How to:
You’ll need the `http-conduit` package for HTTP actions and `base64-bytestring` for encoding credentials. Import them and use `applyBasicAuth` to add credentials to your request.

```Haskell
import Network.HTTP.Simple
import Data.ByteString.Char8 (pack)
import Data.ByteString.Base64 (encode)

-- Construct basic auth header
let username = "user"
let password = "pass"
let auth = encode $ pack (username ++ ":" ++ password)

-- Create your request
request' = parseRequest_ "GET http://example.com/secret"
let request = setRequestHeader "Authorization" ["Basic " <> auth] request'

-- Perform the request
response <- httpLBS request

-- Handle the response
print $ getResponseBody response
```

This will output the API response, if your credentials check out.

## Deep Dive
Basic auth is ancient in web years, designed in the early '90s, and it's as simple as it gets: base64 encoded `username:password` sent in a header. It lacks fancy features like token expiration and, being unencrypted, should always be used over HTTPS.

Alternatives like OAuth provide more secure, granular control. For Haskell, libraries like `http-client` and `wreq` give you more options and flexibility.

Implementation-wise, remember not to hardcode credentials! Use environment variables or a secure vault in production. And since `base64` encoding isn't encryption (anyone can decode it), HTTPS isn't just a good idea, it’s a must.

## See Also
- Haskell `http-conduit` docs: https://hackage.haskell.org/package/http-conduit
- `base64-bytestring` for encoding: https://hackage.haskell.org/package/base64-bytestring
- For tight security, read about OAuth2 in Haskell: https://hackage.haskell.org/package/hoauth2
- Read on best practices for storing secrets: https://www.yesodweb.com/book/security-considerations
