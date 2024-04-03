---
date: 2024-01-20 17:59:51.558220-07:00
description: "W Elm, wysy\u0142anie zapytania HTTP to spos\xF3b na po\u0142\u0105\
  czenie si\u0119 z serwerem i pobranie lub wys\u0142anie danych. Programi\u015Bci\
  \ robi\u0105 to, aby ich aplikacje mog\u0142y\u2026"
lastmod: '2024-03-13T22:44:35.318644-06:00'
model: gpt-4-1106-preview
summary: "W Elm, wysy\u0142anie zapytania HTTP to spos\xF3b na po\u0142\u0105czenie\
  \ si\u0119 z serwerem i pobranie lub wys\u0142anie danych."
title: "Wysy\u0142anie \u017C\u0105dania HTTP"
weight: 44
---

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
