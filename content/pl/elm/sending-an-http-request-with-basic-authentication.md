---
title:                "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
html_title:           "Elm: Wysyłanie żądania http z podstawowym uwierzytelnieniem"
simple_title:         "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Wysyłanie żądania HTTP z podstawową autoryzacją jest sposobem na uwierzytelnienie żądania do serwera za pomocą podanych danych logowania. Programiści wykorzystują to, aby uzyskać dostęp do zabezpieczonych zasobów lub przesłać poufne informacje.

## Jak to zrobić:
Przykładowy kod w Elm wykorzystujący wysyłanie żądania HTTP z podstawową autoryzacją:
```
import Http
import Basics.Time as Time exposing (Time)
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (..)

request : Time -> Time -> Time -> Http.Request
request startTime endTime syncTime =
    let
        body =
            [ ("start_time", Json.Encode.float startTime)
            , ("end_time", Json.Encode.float endTime)
            , ("sync_time", Json.Encode.float syncTime)
            ]
                |> Json.Encode.object
    in
        Http.post
            { url = "https://example.com/api"
            , body = body
            , expect = Http.expectJson Decode.int
            }
```

Możesz również dodać nagłówek autoryzacyjny do żądania w następujący sposób:
```
Http.post
    { url = "https://example.com/api"
    , body = body
    , headers = [ ( "Authorization", "Basic dXNlcjpwYXNzd29yZA==" ) ]
    , expect = Http.expectJson Decode.string
    }
```

## Lewe kolano:
Wysyłanie żądania HTTP z podstawową autoryzacją było powszechnym sposobem uwierzytelniania żądań do serwera przed wprowadzeniem bardziej zaawansowanych metod takich jak tokeny uwierzytelniające. Jednak nie jest to zalecane dla zabezpieczonej komunikacji, ponieważ dane logowania są przesyłane w formie niezaszyfrowanej przez sieć.

## Zobacz też:
- [Dokumentacja zakresu Elm Http](https://package.elm-lang.org/packages/elm/http/latest/)
- [Przewodnik po podstawach Elm](https://guide.elm-lang.org/)