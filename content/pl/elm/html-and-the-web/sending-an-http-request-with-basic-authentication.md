---
date: 2024-01-20 18:01:47.790947-07:00
description: "How to (Jak to zrobi\u0107): W Elm, by wys\u0142a\u0107 \u017C\u0105\
  danie HTTP z podstawowym uwierzytelnianiem, u\u017Cywamy modu\u0142u `Http` i dodajemy\
  \ nag\u0142\xF3wek `Authorization` z\u2026"
lastmod: '2024-03-13T22:44:35.321461-06:00'
model: gpt-4-1106-preview
summary: "W Elm, by wys\u0142a\u0107 \u017C\u0105danie HTTP z podstawowym uwierzytelnianiem,\
  \ u\u017Cywamy modu\u0142u `Http` i dodajemy nag\u0142\xF3wek `Authorization` z\
  \ zakodowanymi danymi logowania."
title: "Wysy\u0142anie zapytania http z podstawow\u0105 autoryzacj\u0105"
weight: 45
---

## How to (Jak to zrobić):
W Elm, by wysłać żądanie HTTP z podstawowym uwierzytelnianiem, używamy modułu `Http` i dodajemy nagłówek `Authorization` z zakodowanymi danymi logowania. Oto przykład:

```Elm
import Http
import Base64

type Msg
    = GotResponse (Result Http.Error String)

basicAuth : String -> String -> Http.Header
basicAuth username password =
    let
        encodedCredentials =
            Base64.encode (username ++ ":" ++ password)
    in
    Http.header "Authorization" ("Basic " ++ encodedCredentials)

sendRequest : Cmd Msg
sendRequest =
    Http.get
        { url = "https://example.com/protected"
        , expect = Http.expectString GotResponse
        , headers = [ basicAuth "username" "password" ]
        }
```

Sample output could be the content of the protected resource or an error message depending on the response.

## Deep Dive (Dogłębna analiza):
Podstawowe uwierzytelnianie HTTP to stara, ale prosta metoda zabezpieczania żądań HTTP. Zyskało popularność, bo łatwo się je implementuje. Należy jednak pamiętać, że metoda ta nie jest najbezpieczniejsza — dane są przesyłane w postaci zakodowanej, ale niezaszyfrowanej. Alternatywy jak OAuth lub tokeny API są często wybierane dla większego bezpieczeństwa.

W Elm, implementacja podstawowego uwierzytelniania opiera się o moduł `Http` i dodatkowe moduły jak `Base64` do kodowania danych. Kodujemy parę `username:password` i dodajemy jako nagłówek `Authorization`. Elm dba o to, by interakcje były typu bezpieczne i łatwe do obsługi w języku Elm – stąd użycie `Result` w danych wejściowych odpowiedzi, by łatwiej obsłużyć błędy.

## See Also (Zobacz także):
- Elm HTTP package: [http://package.elm-lang.org/packages/elm/http/latest](http://package.elm-lang.org/packages/elm/http/latest)
- Elm Base64 package: [http://package.elm-lang.org/packages/truqu/elm-base64/latest](http://package.elm-lang.org/packages/truqu/elm-base64/latest)
- HTTP Authentication: Basic and Digest Access Authentication RFC: [https://tools.ietf.org/html/rfc7617](https://tools.ietf.org/html/rfc7617)
- Elm lang guide for HTTP: [https://guide.elm-lang.org/effects/http.html](https://guide.elm-lang.org/effects/http.html)
