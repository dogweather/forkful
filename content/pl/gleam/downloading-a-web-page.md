---
title:                "Pobieranie strony internetowej"
date:                  2024-01-20T17:44:12.798518-07:00
model:                 gpt-4-1106-preview
simple_title:         "Pobieranie strony internetowej"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why? / Czym i Dlaczego?
Pobieranie strony internetowej to proces wyciągania zawartości HTML bezpośrednio z sieci. Programiści robią to dla analizy danych, testowania aplikacji lub automatyzacji zadań.

## How to / Jak to zrobić:
```gleam
import gleam/http
import gleam/httpc

pub fn download_web_page(url: String) -> httpc.Result {
  httpc.get(url)
}

// Usage
fn main() { 
  let result = download_web_page("https://example.com".to_string())
  case result {
    Ok(response) -> io.println(response.body) 
    Error(error) -> io.println(error)
  }
}
```

Sample output:
```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>
```

## Deep Dive / W Głąb Tematu
Downloading web pages is as old as the internet. It began with simple HTTP GET requests. Now, it's often part of web scraping, automation, or RESTful API interactions.

Gleam, although a newer player in the ecosystem, makes this task simple with the `httpc` module. Alternatives in other languages include Python's `requests` or JavaScript's `fetch`. In Gleam, use matching on `Result` to handle HTTP errors neatly.

The `httpc.get` function initiates a GET request which fetches the HTML content. Implementation involves sending an HTTP request and parsing the response.

## See Also / Zobacz Również
- Gleam HTTP documentation: [https://hexdocs.pm/gleam_http/](https://hexdocs.pm/gleam_http/)
- HTTP protocol: [https://developer.mozilla.org/en-US/docs/Web/HTTP](https://developer.mozilla.org/en-US/docs/Web/HTTP)
