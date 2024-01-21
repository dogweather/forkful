---
title:                "Wysyłanie zapytania http z podstawową autoryzacją"
date:                  2024-01-20T18:01:44.855318-07:00
model:                 gpt-4-1106-preview
simple_title:         "Wysyłanie zapytania http z podstawową autoryzacją"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why? / Co i Dlaczego?
Wysyłanie żądania HTTP z podstawowym uwierzytelnieniem to proces dołączania loginu i hasła do żądania sieciowego. Programiści używają tego do komunikacji z zabezpieczonymi endpointami API, wymagającymi uwierzytelnienia.

## How to: / Jak to zrobić:
W Gleam do wysłania żądania z podstawowym uwierzytelnieniem można użyć popularnej biblioteki HTTP. Oto przykład:

```gleam
import gleam/http
import gleam/httpc
import gleam/base64

pub fn send_authenticated_request() {
  let username = "user"
  let password = "pass"
  let credentials = base64.encode(username ++ ":" ++ password)
  let auth_header = "Basic " ++ credentials

  let headers = [
    {"Authorization", auth_header}
  ]

  try request = httpc.post(
    "https://example.com/api/resource",
    headers,
    http.Body("")
  )
  request
}
```

Przykładowe wyjście może wyglądać tak:

```gleam
Ok(#http.Response(200, [], "Resource content"))
```

## Deep Dive / Dogłębna analiza:
Wczesne wersje protokołu HTTP nie miały wbudowanych mechanizmów uwierzytelniania. Basic authentication została dodana w HTTP 1.0 i jest prostym, lecz mniej bezpiecznym sposobem uwierzytelniania. Dane są kodowane w Base64, co nie jest metodą szyfrowania, a jedynie kodowania. Alternatywą jest Digest Authentication, OAuth, lub stosowanie tokenów, np. JWT (JSON Web Tokens).

Podstawowe uwierzytelnienie w HTTP jest proste w implementacji, wystarczy dodać nagłówek 'Authorization' z poświadczeniami zakodowanymi w Base64. W przypadku Gleama i większości współczesnych języków, istnieją gotowe biblioteki, ułatwiające ten proces i zapewniające większe bezpieczeństwo, np. poprzez obsługę HTTPS.

## See Also / Zobacz również:
- Dokumentacja Gleam HTTP: [https://hexdocs.pm/gleam_http/](https://hexdocs.pm/gleam_http/)
- RFC 7617, 'The 'Basic' HTTP Authentication Scheme': [https://tools.ietf.org/html/rfc7617](https://tools.ietf.org/html/rfc7617)