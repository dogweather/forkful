---
title:                "Sending an http request with basic authentication"
html_title:           "Gleam recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why?

Sending an HTTP request with basic authentication entails embedding a username and password in an HTTP header to password-protect server resources. Programmers do it to enhance web security and preserve data privacy.

## How to:

Here is how to send an HTTP request with basic authentication in Gleam:

```Gleam
import gleam/http
import gleam/http/request
import gleam/http/response

pub fn send_request() {
  let authentication = request.Authentication.basic("username", "password")
  let my_request = request.get("http://localhost:3000/secure-page")
    |> request.with_authentication(authentication)

  case http.send(my_request) {
    Ok(response) ->
      response
      |> response.body
      |> io.println

    Error(error) ->
      error
      |> http.Error.to_string
      |> io.println
  }
}
```

## Deep Dive:

Historically, HTTP Basic Authentication was proposed in 1999 under RFC 2617 as the simplest technique for enforcing access controls to web resources. While it remains popular due to its simplicity, be aware that it's not the most secure method as credentials are transported in plaintext, so always use HTTPS.

An alternative to basic authentication is token-based authentication, which typically offers more robust security features. OAuth, for example, is a powerful and flexible protocol that is widely adopted.

In terms of implementation under the hood, the Gleam HTTP library uses Erlang's `httpc` client. When preparing a request, the `request.with_authentication` function merges the Authentication header into the request's existing headers. The library then base64-encodes the combined username and password before sending as part of the HTTP header.

## See Also:

To learn more details about HTTP Basic authentication, read the official basics in the [RFC 2617 doc](https://tools.ietf.org/html/rfc2617#section-2).

More about Gleam’s HTTP handling can be found in the [Gleam HTTP library docs](https://hexdocs.pm/gleam_http/gleam/http/index.html).

For an alternative, secure authentication method, read about [OAuth](https://oauth.net/2/).