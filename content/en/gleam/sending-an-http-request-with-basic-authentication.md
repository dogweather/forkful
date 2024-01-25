---
title:                "Sending an HTTP request with basic authentication"
date:                  2024-01-20T18:01:33.635455-07:00
model:                 gpt-4-1106-preview
simple_title:         "Sending an HTTP request with basic authentication"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why?

Sending an HTTP request with basic authentication means you're slapping a username and password on your request, like a secret handshake to access a web resource. Programmers do it to make sure only those in-the-know (i.e., with the right credentials) get through.

## How to:

In Gleam, you'll use the `gleam/http` library. Here's a pared-down example:

```gleam
import gleam/http.{Request, BasicAuth, get}

pub fn fetch_resource_with_basic_auth() {
  let auth = BasicAuth(
    username: "awesome_dev",
    password: "superSecret123",
  )
  let request = Request(
    method: Get,
    url: "https://api.example.com/protected",
    headers: [],
    body: None,
    basic_auth: Some(auth),
  )

  assert Ok(response) = get(request)
  response
}
```

What you’ll see when you run it:

```gleam
Ok(#{
  status: 200,
  headers: [...],
  body: "Here's your secret data!"
})
```

Or, if the creds are wrong:

```gleam
Ok(#{
  status: 401,
  headers: [...],
  body: ""
})
```

## Deep Dive

Back in the day, basic authentication was one of the earliest methods for securing web communications; it's like an old padlock—simple but not the most secure. 

Alternatives? You've got OAuth for more complex scenarios, or token-based auth for a compromise between simplicity and security. 

Implementation wise, basic auth in HTTP shoves a Base64 encoded string (your username and password combo) into an 'Authorization' header. It's not encrypted, hence why it's basic and not recommended for sensitive stuff without HTTPS at least. Also, don't hardcode credentials in your code; use environment variables or a secure vault service.

## See Also

Dive deeper into:

- [HTTP authentication on MDN](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [Safe storage of app secrets](https://learn.microsoft.com/en-us/aspnet/core/security/app-secrets)