---
title:                "Sending an http request with basic authentication"
html_title:           "Fish Shell recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Mastering HTTP Requests with Basic Authentication in Gleam

## What & Why?
Sending an HTTP request with basic authentication in Gleam is the process of transmitting data over the web while securing it with user credentials. Programmers do it to facilitate secure communication in web services.

## How to:

Here's the drill.

```Gleam
import gleam/http.{client, headers, request, credentials}

let creds = credentials.basic("username", "password")

let header_creds =
  credentials
  |> headers.bearer_authorization
  |> list.to_tuple

let req =
  request.new(Url.parse("https://myapp.com").unwrap())
  |> request.with_headers(header_creds)

let _ = client.send(req)
```

This chunk of code sends an HTTP request with basic authentication by appending a header (`header_creds`) containing encoded credentials to the request (`req`). The `client.send(req)` is used to fire off the request.

## Deep Dive

This convention originated from the early days of web development to shield simple transactions over HTTP. Today alternatives include Digest Authentication, OAuth2, and JWTs, but Basic Authentication remains a straightforward, easy to implement method, provided a secure (HTTPS) connection is used.

The `credentials.basic()` function in Gleam applies Base64 encoding to the provided credentials and mind you, this isn't encryption - just encoding, hence the need for a secure connection to ensure password security.

The encoded credentials are set into headers using the `headers.bearer_authorization` function and thereafter pushed into the request via `request.with_headers()`.

## See Also

- Gleam's HTTP library and examples on GitHub: https://github.com/gleam-lang/http
- Mozilla's document on HTTP authentication: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
- Internet Engineering Task Force (IETF) Basic Authentication's standard: https://tools.ietf.org/html/rfc7617