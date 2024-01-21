---
title:                "Надсилання HTTP-запиту"
date:                  2024-01-20T17:59:35.376873-07:00
model:                 gpt-4-1106-preview
simple_title:         "Надсилання HTTP-запиту"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (Що & Чому?)
Sending an HTTP request means asking a web service for data or performing an action over the internet. Programmers do it to interact with APIs, fetch resources, or communicate between systems.

## How to: (Як це зробити:)
```Gleam
import gleam/http
import gleam/httpc

pub fn main() {
  case httpc.send(http.Request(to: "http://example.com")) {
    Ok(response) ->
      io.println("Response received: " ++ response.body)
    
    Error(error) ->
      io.println("Request failed: " ++ error)
  }
}
```
Sample Output:
```
Response received: <html>...</html>
```

## Deep Dive (Поглиблене вивчення)
Sending HTTP requests has been crucial since the dawn of web programming, enabling distributed computing and services. Alternatives like WebSocket or GraphQL exist, but HTTP remains the backbone of web communication. In Gleam, HTTP request handling is built with reliability and type safety in mind, leveraging Erlang's robustness.

## See Also (Дивіться також):
- Gleam HTTP documentation: [https://hexdocs.pm/gleam_http/](https://hexdocs.pm/gleam_http/)
- Erlang's `httpc` module: [https://erlang.org/doc/man/httpc.html](https://erlang.org/doc/man/httpc.html)
- RESTful API guidelines: [https://restfulapi.net/](https://restfulapi.net/)
- Introduction to GraphQL: [https://graphql.org/learn/](https://graphql.org/learn/)