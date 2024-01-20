---
title:                "Sending an http request with basic authentication"
html_title:           "Fish Shell recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why?

HTTP requests with basic authentication are a way for Haskell programs to interact with web servers requiring username/password combinations. Programmers use this to access protected data or APIs with these security measures in place.

## How to:

Let's use the `http-conduit` and `wreq` packages to send HTTP requests with basic authentication. Install them with:

```Haskell
cabal install http-conduit wreq
```

Import these packages:

```Haskell
import Network.HTTP.Simple
import Network.HTTP.Client.Conduit (applyBasicAuth)
import Control.Lens
import Network.Wreq
```

For `http-conduit`, create a `Request` with `applyBasicAuth`:

```Haskell
request <- parseRequest "http://httpbin.org/basic-auth/user/passwd"
let request' = applyBasicAuth "user" "passwd" request
response <- httpBS request'
```

To confirm, print status and the first 100 bytes of the response:

```Haskell
putStrLn $ "The status code was: " ++ show (getResponseStatusCode response)
putStrLn $ B.unpack $ B.take 100 $ getResponseBody response
```

For `wreq`, use `basicAuth` in `options`:

```Haskell
let opts = defaults & auth ?~ basicAuth "user" "password"
r <- getWith opts "http://httpbin.org/basic-auth/user/password"
```

Print response:

```Haskell
print r
```

## Deep Dive

Basic authentication existed since HTTP/1.0 as a convenient method for sending authenticated requests - but it's not secure without SSL/TLS due to credentials being sent in plain text. Newer standards like OAuth2 are recommended nowadays but basic authentication remains in use for its simplicity.

`http-conduit` has been around as a go-to for Haskell HTTP requests. It's powerful and flexible. `wreq`, however, focuses on making common tasks convenient like accessing authenticated endpoints.

`applyBasicAuth` and `basicAuth` both encode the username/password pair into a header following the Basic Authentication Schema. The server decodes this from the header to authenticate the request.

## See Also

To further explore these concepts, check the following links:

* Basic Authentication on Wikipedia: https://en.wikipedia.org/wiki/Basic_access_authentication
* The http-conduit package: https://hackage.haskell.org/package/http-conduit
* The wreq package: https://hackage.haskell.org/package/wreq.
* OAuth2 for securing web APIs: https://oauth.net/2/