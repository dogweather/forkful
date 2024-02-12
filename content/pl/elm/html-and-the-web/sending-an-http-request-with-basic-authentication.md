---
title:                "Wysyłanie zapytania http z podstawową autoryzacją"
aliases:
- /pl/elm/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:01:47.790947-07:00
model:                 gpt-4-1106-preview
simple_title:         "Wysyłanie zapytania http z podstawową autoryzacją"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Wysyłanie żądania HTTP z podstawowym uwierzytelnianiem to metoda, gdzie login i hasło są dodawane do nagłówków żądania w celu dostępu do zabezpieczonych zasobów. Programiści używają tego, by umożliwić aplikacjom komunikację z serwerami wymagającymi autoryzacji.

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
