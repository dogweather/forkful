---
title:                "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
html_title:           "Arduino: Wysyłanie żądania http z podstawowym uwierzytelnieniem"
simple_title:         "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Wysyłanie żądania HTTP z podstawowym uwierzytelnieniem to kwestia przesyłania identyfikatora użytkownika i hasła jako część żądania HTTP. Programiści robią to, by uzyskać dostęp do zabezpieczonych zasobów.

## Jak to zrobić:

W Elixir, możemy użyć takich bibliotek jak HTTPoison do wysyłania żądań HTTP. Dla podstawowego uwierzytelnienia, przygotowujemy 'header' z identyfikatora użytkownika i hasła, zakodowane w base64.

```elixir
defmodule MyHTTPClient do
  def get(url, username, password) do
    headers = [{"Authorization", "Basic #{:base64.encode_to_string("#{username}:#{password}")}"}]
    HTTPoison.get(url, headers)
  end
end

{:ok, response} = MyHTTPClient.get("http://example.com", "user", "pass")
IO.inspect(response.status_code)
```

## Głębsze spojrzenie:

Historia podstawowego uwierzytelnienia sięga czasów, kiedy protokół HTTP był jeszcze w początkowej fazie rozwoju. Dziś jest to jedna z najprostszych form uwierzytelnienia, ale nie jest zalecana do używania bez szyfrowania TLS/SSL ze względu na potencjalne naruszenie bezpieczeństwa.

Alternatywą dla podstawowego uwierzytelnienia jest uwierzytelnienie za pomocą tokenu, takie jak JWT (JSON Web Token), który jest bardziej bezpieczny i oferuje więcej funkcji.

Szczegóły implementacji w Elixir sprowadzają się do tworzenia 'headera' uwierzytelnienia, kodując dane uwierzytelniające do base64, a następnie dodając je do żądania HTTP.

## Zobacz też:

1. [HTTPoison](https://hexdocs.pm/httpoison/HTTPoison.html): Dokumentacja biblioteki HTTPoison.
2. [Base64](https://hexdocs.pm/elixir/Base.html): Moduł Base64 w Elixir.
3. [Uwierzytelnianie JWT](https://jwt.io/): Informacje o JWT, alternatywie dla podstawowego uwierzytelnienia.