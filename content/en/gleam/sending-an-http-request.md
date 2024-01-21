---
title:                "Sending an http request"
date:                  2024-01-20T17:59:42.719306-07:00
model:                 gpt-4-1106-preview
simple_title:         "Sending an http request"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?
Sending an HTTP request is how your program asks for data from somewhere else on the internet. Programmers do this to interact with web services, grab fresh data, or to integrate external APIs into their apps.

## How to:
In Gleam, we don't have a built-in HTTP client yet, so let's use the `gleam_http` library. Here's a simple GET request:

```gleam
import gleam/http.{Response, Error}
import gleam/httpc

pub fn main() -> Result(Response, Error) {
  // Send a GET request to example.com
  httpc.send(httpc.Request(
    method: httpc.Get,
    url: "http://example.com",
    headers: [],
    body: httpc.Empty,
  ))
}
```

Running this will send a request to example.com and return the response. Remember, you'll need to handle the result to actually make use of it.

## Deep Dive
Historically, sending HTTP requests was a task for specialized tools like `curl` or libraries in other languages. It's fairly new that languages themselves are incorporating smooth HTTP request functionalities. Alternatives for sending HTTP requests in Gleam include third-party libraries like `gleam_http` and platform-specific bindings.

Regarding implementation, there are two parts: constructing the request and receiving the response. Requests have methods (GET, POST, etc.), URLs, headers, and bodies while responses carry status codes, headers, and bodies as well.

Gleam's type system and pattern matching shine here, enabling exhaustive error handling and clear parsing of responses. This isn't just firing data into the void and hoping for the best; it's a controlled, structured conversation.

## See Also
- [Gleam's HTTP documentation](https://hexdocs.pm/gleam_httpc/)
- [HTTP RFC 7231](https://tools.ietf.org/html/rfc7231) for the nitty-gritty on HTTP.
- [Gleam's standard library](https://hexdocs.pm/gleam_stdlib/) for other networking features.