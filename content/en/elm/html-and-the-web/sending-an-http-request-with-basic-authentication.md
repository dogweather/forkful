---
date: 2024-01-20 18:01:30.602993-07:00
description: "Sending an HTTP request with basic authentication involves attaching\
  \ login credentials (username and password) to the request headers to access protected\u2026"
lastmod: '2024-03-13T22:45:00.009235-06:00'
model: gpt-4-1106-preview
summary: "Sending an HTTP request with basic authentication involves attaching login\
  \ credentials (username and password) to the request headers to access protected\u2026"
title: Sending an HTTP request with basic authentication
---

{{< edit_this_page >}}

## What & Why?

Sending an HTTP request with basic authentication involves attaching login credentials (username and password) to the request headers to access protected resources. Programmers use it for simple authentication on HTTP APIs where the overhead of more complex systems isn't needed.

## How to:

Elm makes HTTP requests using the `Http` package. To add basic auth, you encode the credentials and include them in the request headers.

```Elm
import Http
import Base64

type alias Model = { ... }
type Msg = HttpRequestCompleted (Result Http.Error String)

-- Encode username and password
basicAuthHeader : String -> String -> Http.Header
basicAuthHeader username password =
    let
        credentials = username ++ ":" ++ password
        encodedCredentials = Base64.encode credentials
    in
    Http.header "Authorization" ("Basic " ++ encodedCredentials)

-- Make the HTTP request
sendRequestWithBasicAuth : Cmd Msg
sendRequestWithBasicAuth =
    let
        url = "https://example.com/protected/resource"
        request =
            Http.request
                { method = "GET"
                , headers = [ basicAuthHeader "myUsername" "myPassword" ]
                , url = url
                , body = Http.emptyBody
                , expect = Http.expectString (HttpRequestCompleted)
                , timeout = Nothing
                , tracker = Nothing
                }
    in
    Http.send HttpRequestCompleted request
```

When the above function is called, Elm will perform a GET request to the specified URL with the Authorization header set to the encoded username and password.

## Deep Dive

Elm's approach to HTTP requests is a reflection of the language's overall philosophy: safe, easy to maintain, and understandable. The `Http` package encapsulates requests in a way that deals with the Elm architecture.

Basic authentication is as old as the web itself, part of the original HTTP specification (RFC 7617). It's straightforward but not very secure since the credentials are only base64-encoded, not encrypted. Therefore, it's critical to use HTTPS to encode the transmission.

Alternatives to basic auth include OAuth, tokens like JWT, or API keys, each coming with increased complexity and improved security. Elm supports these methods too but often requires additional packages or custom encoders and decoders.

## See Also

- Elm's Official `Http` package documentation: [package.elm-lang.org/packages/elm/http/latest](https://package.elm-lang.org/packages/elm/http/latest)
- Elm's `Base64` package source: [package.elm-lang.org/packages/truqu/elm-base64/latest](https://package.elm-lang.org/packages/truqu/elm-base64/latest)
- RFC 7617, The 'Basic' HTTP Authentication Scheme: [tools.ietf.org/html/rfc7617](https://tools.ietf.org/html/rfc7617)
