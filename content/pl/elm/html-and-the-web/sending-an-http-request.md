---
title:                "Wysyłanie żądania HTTP"
aliases:
- /pl/elm/sending-an-http-request.md
date:                  2024-01-20T17:59:51.558220-07:00
model:                 gpt-4-1106-preview
simple_title:         "Wysyłanie żądania HTTP"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? / Co i Dlaczego?
W Elm, wysyłanie zapytania HTTP to sposób na połączenie się z serwerem i pobranie lub wysłanie danych. Programiści robią to, aby ich aplikacje mogły komunikować się z zewnętrznymi API, pobierać aktualizacje lub przekazywać informacje.

## How to / Jak to zrobić:
```Elm
import Http
import Json.Decode as Decode

type alias User =
    { id : Int
    , name : String
    }

userDecoder : Decode.Decoder User
userDecoder =
    Decode.map2 User
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)

fetchUser : Cmd Msg
fetchUser =
    Http.get
        { url = "https://api.example.com/users/1"
        , expect = Http.expectJson GotUser userDecoder
        }

type Msg
    = GotUser (Result Http.Error User)

-- Elm Main Update function and subscriptions would be here
```

Sample Output:
```Elm
GotUser (Ok { id = 1, name = "John Doe" })
```

## Deep Dive / Szczegółowa analiza:
Wysyłanie zapytania HTTP w Elmie sięga czasów Elm 0.18, kiedy to funkcjonalności te zostały znacznie uproszczone. Elm 0.19 dalej ulepsza te procesy, zapewniając silne typowanie i bezpieczeństwo w użyciu.

Elm używa `Cmd` do obsługi efektów ubocznych, takich jak zapytania HTTP, co pozwala na izolację efektów od czystych funkcji. Możesz użyć `Http.get`, `Http.post` i innych funkcji z modułu `Http`, by rozmawiać z serwerami.

Co więcej, zapytania HTTP w Elm korzystają z systemu dekoderów JSON (takich jak `userDecoder` w powyższym przykładzie), które pozwalają na bezpieczne i predyktywne parsowanie odpowiedzi serwera.

Alternatywą dla HTTP w Elm jest WebSockets dla real-time aplikacji, ale to całkiem inna historia.

## See Also / Zobacz również:
- Elm HTTP package documentation: [https://package.elm-lang.org/packages/elm/http/latest/](https://package.elm-lang.org/packages/elm/http/latest/)
- Json.Decode documentation: [https://package.elm-lang.org/packages/elm/json/latest/Json-Decode](https://package.elm-lang.org/packages/elm/json/latest/Json-Decode)
- Elm Guide - HTTP: [https://guide.elm-lang.org/effects/http.html](https://guide.elm-lang.org/effects/http.html)
