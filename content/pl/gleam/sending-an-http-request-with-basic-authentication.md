---
title:                "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
html_title:           "Arduino: Wysyłanie żądania http z podstawowym uwierzytelnieniem"
simple_title:         "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?

Wysyłanie żądania HTTP z podstawowym uwierzytelnieniem to metoda umożliwiająca przesyłanie pewnych informacji (np. nazwy użytkownika i hasła) niezbędnych do dostępu do zasobu. Programiści to robią, aby chronić swoje zasoby przed nieuprawnionym dostępem.

## Jak to zrobić:

Oto przykładowy kod, który demonstruje, jak wymagać podstawowego uwierzytelnienia HTTP w Gleam:

```Gleam
import gleam/httpc
import gleam/bit_builder.{BitBuilder}
import gleam/base64

fn basic_auth_header(user: String, pass: String) -> httpc.Header {
  BitBuilder.new()
  |> BitBuilder.append_string(user)
  |> BitBuilder.append_string(":")
  |> BitBuilder.append_string(pass)
  |> BitBuilder.to_binary()
  |> base64.encode()
  |> (auth) { Ok(httpc.header("Authorization", tuple("Basic", auth))) }
}

fn request_with_auth(url: httpc.Url, user: String, pass: String) {
  auth_header = basic_auth_header(user, pass)

  httpc.get(url, [auth_header])
  |> result.unwrap()
  |> httpc.send()
}
```

## Na głęboko:

### Kontekst historyczny
Podstawowe uwierzytelnianie HTTP jest jednym z najstarszych sposobów uwierzytelniania w Internecie, wprowadzonym w standardzie HTTP 1.0 w 1996 roku.

### Alternatywy
Alternatywą dla podstawowego uwierzytelnienia HTTP jest uwierzytelnienie typu "bearer", szczególnie używane przy tokenach JWT, uwierzytelnianie Digest i uwierzytelnianie OAuth.

### Szczegóły implementacji
Wartości użytkownika i hasła są konkatenowane i następnie przekształcane na systemy binarne. Ta wartość jest następnie kodowana w Base64 i dodana do nagłówka autoryzacji.

## Zobacz też:

- Dokumentacja Gleam `httpc` i `base64`: https://hexdocs.pm/gleam_stdlib/
- Specyfikacja uwierzytelnienia HTTP: https://tools.ietf.org/html/rfc7617
- Alternatywy dla podstawowego uwierzytelnienia HTTP: https://auth0.com/blog/understanding-http-authentication-schemes/