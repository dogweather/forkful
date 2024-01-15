---
title:                "Wysyłanie żądania http z podstawową autoryzacją"
html_title:           "Elm: Wysyłanie żądania http z podstawową autoryzacją"
simple_title:         "Wysyłanie żądania http z podstawową autoryzacją"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach, duża część aplikacji internetowych korzysta z uwierzytelniania podstawowego przy wykonywaniu żądań HTTP. Jest to sposób na zabezpieczenie aplikacji przed niepożądanym dostępem i zapewnienie prywatności użytkowników. Jeśli chcesz nauczyć się, jak wysyłać żądanie HTTP z uwierzytelnianiem podstawowym w Elm, ten artykuł jest dla Ciebie!

## Jak to zrobić

W Elm istnieje moduł o nazwie `Http` który pozwala nam na wykonywanie żądań HTTP. Aby wysłać żądanie z uwierzytelnieniem podstawowym, musimy dostarczyć odpowiednie dane uwierzytelniające w nagłówku `Authorization` naszego żądania. Poniżej znajduje się przykładowy kod:

```Elm
import Http
import Basics exposing (..)
import Json.Encode exposing (..)

type alias AuthData =
    { username : String
    , password : String
    }

loginUrl : String
loginUrl =
    "https://mojaserwer.pl/login"

authData : AuthData
authData =
    { username = "mojlogin"
    , password = "haslom"
    }

headers : List (String, String)
headers =
    [ ( "Authorization", "Basic " ++ Base64.encode (Json.Encode.encode authData) ) ]

loginRequest : Http.Request
loginRequest =
    Http.post loginUrl
        (Http.jsonBody (Json.Encode.encode ()))
        |> Http.headers headers

main =
    Http.toTask loginRequest
```

W powyższym przykładzie, definiujemy funkcję `loginRequest`, która jest odpowiedzialna za wysłanie żądania POST do naszego serwera logowania. W nagłówku `Authorization`, używamy funkcji `Base64.encode` do zakodowania danych uwierzytelniających w formacie JSON. Następnie wywołujemy funkcję `Http.toTask`, która przekształca nasze żądanie do obiektu typu `Task`, który możemy wykonać za pomocą funkcji `Task.perform` lub `Task.attempt`.

## Deep Dive

W przypadku uwierzytelniania podstawowego, dane uwierzytelniające są przesyłane w formacie tekstowy, ale istnieją inne rodzaje uwierzytelnienia, które używają innych sposobów szyfrowania i przesyłania danych uwierzytelniających. W Elm istnieje również moduł `HTTP` który pozwala na wysyłanie żądań z innymi metodami uwierzytelnienia, takimi jak uwierzytelnienie tokenowe czy uwierzytelnianie OAuth.

## Zobacz również

- Dokumentacja Elm dla modułu `Http`: https://package.elm-lang.org/packages/elm/http/latest/
- Przewodnik po uwierzytelnianiu HTTP w języku angielskim: https://www.httpwatch.com/httpgallery/authentication/
- Przykład zastosowania uwierzytelnienia HTTP w aplikacjach Elm: https://dev.to/kayashaolu/basic-http-authentication-in-elm-48a6