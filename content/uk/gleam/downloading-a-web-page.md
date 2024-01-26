---
title:                "Завантаження веб-сторінки"
date:                  2024-01-20T17:44:10.435501-07:00
model:                 gpt-4-1106-preview
simple_title:         "Завантаження веб-сторінки"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why? (Що і Чому?)
Завантаження веб-сторінки — це процес отримання її вмісту через інтернет. Програмісти роблять це для аналізу даних, автоматизації завдань чи інтеграції з веб-сервісами.

## How to (Як це зробити):
```gleam
import gleam/http
import gleam/httpc

pub fn main() {
  case httpc.get("https://example.com") {
    Ok(response) -> 
      io.println("Page downloaded! Status: " ++ response.status_code.to_string())
    
    Error(error) ->
      io.println("Failed to download page: " ++ error)
  }
}
```
Sample output:
```
Page downloaded! Status: 200
```

## Deep Dive (Занурення у глибину):
Downloading web pages can be traced back to the beginning of the web itself. In Gleam, we use the `http` and `httpc` modules for clean, asynchronous HTTP requests. Alternatives in other languages include Python's `requests` or JavaScript's `fetch`. Gleam, being functional and type-safe, offers reliable error handling allowing for robust applications. Understanding the HTTP protocol and status codes can greatly improve how you implement page downloading.

## See Also (Дивіться також):
- Gleam HTTP library documentation: https://hexdocs.pm/gleam_http/
- HTTP status codes explanation: https://developer.mozilla.org/en-US/docs/Web/HTTP/Status
- RFC 7230 (HTTP/1.1 Protocol): https://tools.ietf.org/html/rfc7230
