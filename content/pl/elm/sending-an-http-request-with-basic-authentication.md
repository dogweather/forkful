---
title:                "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
html_title:           "Arduino: Wysyłanie żądania http z podstawowym uwierzytelnieniem"
simple_title:         "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Wysyłanie żądania HTTP z podstawowym uwierzytelnieniem to możliwość dostępu do chronionych zasobów sieciowych poprzez podanie danych logowania. Programiści robią tak, aby bezpiecznie dostęp do ważnych danych lub funkcji.

## Jak to zrobić:

```
Elm

import Http
import Http.BasicAuth as BasicAuth

request : Http.Request String
request =
    Http.get
        { url = "https://twoja.strona/api"
        , expect = Http.expectString Ok BadUrl
        , headers = [ BasicAuth.header "NazwaUżytkownika" "Hasło" ]
        }
```

Po uruchomieniu kodu, Elm wyśle żądanie GET na podany URL. W razie powodzenia otrzymasz string, który może być przetworzony na potrzeby twojego programu.

## Głębsze zrozumienie

Podstawowe uwierzytelnienie HTTP, wprowadzone po raz pierwszy w specyfikacji HTTP/1.0 w 1996 roku, jest jednym z najprostszych metod uwierzytelniania. Alternatywą jest Digest Authentication, który oferuje jeszcze lepszą ochronę kilka podobieństw do Basic Authentication. Mimo to, popularnością zaczyna przeważać uwierzytelnienie oparte na tokenach, takie jak OAuth.

Pamiętaj, że podstawowe uwierzytelnienie przesyła dane uwierzytelniające jako niezaszyfrowane teksty, więc zawsze używaj protokołu HTTPS do ochrony tych danych.

## Zobacz również

- [Dokumentacja Elm HTTP](https://package.elm-lang.org/packages/elm/http/latest/)
- [Podstawowe uwierzytelnienie w Elm](https://github.com/truqu/elm-basic-auth)
- [Jak korzystać z HTTPS w Elm](https://korban.net/posts/elm/2018-07-23-using-https-elm/)