---
title:                "Wysyłanie żądania HTTP"
date:                  2024-01-20T17:59:47.718901-07:00
model:                 gpt-4-1106-preview
simple_title:         "Wysyłanie żądania HTTP"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Wysyłanie żądania HTTP to komunikacja z serwerem w sieci - zadajemy pytanie, oczekujemy odpowiedzi. Programiści to robią, by pobierać dane, wysyłać informacje, integrować serwisy i zarządzać systemami.

## How to: (Jak to zrobić:)
Gleam używa pakietu `gleam_http` do obsługi HTTP. Oto przykładowy kod:

```gleam
import gleam/http
import gleam/http/httpc
import gleam/should

pub fn request_example() {
  let response = httpc.send(http.Request(
    method: http.Get,
    url: "http://httpbin.org/get",
    headers: [],
    body: http.Body(Nil),
  ))

  match response {
    Ok(response) -> should.equal(response.status, 200)
    Error(error) -> io.println("Error: " ++ error)
  }
}
```

Wykonanie tego kodu skutkuje żądaniem HTTP do `httpbin.org` i, jeśli wszystko pójdzie dobrze, zwraca status 200.

## Deep Dive (W głębinie wiedzy)
Żądania HTTP to podstawa komunikacji w Internecie – ich protokół powstał w 1991 roku. Alternatywą dla `gleam_http` jest bezpośrednie używanie bibliotek w języku hosta, np. `hackney` w Erlangu. W działaniu, `gleam_http` konstruuje zapytanie, przesyła je przez sieć, obsługuje odpowiedź i przekształca ją w typy Gleam.

## See Also (Zobacz również)
- Gleam HTTP pakiet: https://hexdocs.pm/gleam_http/
- Dokumentacja `httpc`: https://hexdocs.pm/gleam_http/gleam/http/httpc/
- Przykłady użycia HTTP w Gleam: https://github.com/gleam-lang/http/tree/main/examples