---
title:                "Wysyłanie żądania http z uwierzytelnieniem podstawowym"
html_title:           "Elixir: Wysyłanie żądania http z uwierzytelnieniem podstawowym"
simple_title:         "Wysyłanie żądania http z uwierzytelnieniem podstawowym"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?

Wysyłanie żądania HTTP z podstawowym uwierzytelnianiem polega na przesłaniu informacji uwierzytelniających wraz z żądaniem HTTP, aby potwierdzić tożsamość użytkownika. Programiści często stosują to, aby zabezpieczyć swoją aplikację przed nieautoryzowanym dostępem lub aby uzyskać dostęp do zasobów na zewnętrznym serwerze.

## Jak to zrobić:

```Elixir
HTTPBasic.get("https://example.com", username: "John", password: "secret") 
```

Przykładowy wynik:
```Elixir
{:ok, %HTTPoison.Response{status_code: 200, body: "Witaj John!"}}
```

## Głębsze wgniebienie:

Podstawowe uwierzytelnianie zostało wprowadzone już w 1999 roku jako część specyfikacji protokołu HTTP. Alternatywą dla niego jest uwierzytelnianie za pomocą tokenów, które jest coraz częściej stosowane, głównie ze względu na bezpieczeństwo. W Elixir istnieją różne biblioteki, które pozwalają na wygodne wysyłanie żądań HTTP z uwierzytelnieniem, takie jak HTTPotion czy HTTPoison.

## Zobacz także:

- Dokumentacja Elixir dla biblioteki HTTPoison: https://hexdocs.pm/httpoison/
- Artykuł na temat alternatyw dla podstawowego uwierzytelniania: https://dzone.com/articles/authentication-token-vs-basic-auth-in-plain-engli
- Oficjalna specyfikacja HTTP 1.0 z opisem uwierzytelniania podstawowego: https://tools.ietf.org/html/rfc1945#section-11