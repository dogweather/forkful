---
title:                "Praca z JSON"
date:                  2024-01-19
html_title:           "Bash: Praca z JSON"
simple_title:         "Praca z JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? 
Praca z JSON w Elm polega na przetwarzaniu danych w formacie JSON, co jest standardem w komunikacji API. Programiści robią to, aby łatwo wymieniać dane między aplikacją front-end a back-end.

## How to:
```Elm
import Json.Decode exposing (Decoder, field, int, string, decodeValue)

type alias User = 
    { id : Int
    , name : String
    }

userDecoder : Decoder User
userDecoder =
    Json.Decode.map2 User
        (field "id" int)
        (field "name" string)

jsonString : String
jsonString =
    "{\"id\": 1, \"name\": \"Ada\"}"

decodeResult : Result String User
decodeResult =
    decodeValue userDecoder (Json.Decode.string jsonString)

-- Wartość 'decodeResult' to teraz 'Ok { id = 1, name = "Ada" }'
```

```Elm
import Json.Encode exposing (object, int, string)

user : User
user =
    { id = 1, name = "Ada" }

encodeUser : User -> Json.Encode.Value
encodeUser usr =
    object
        [ ("id", int usr.id)
        , ("name", string usr.name)
        ]

-- Użycie 'encodeUser user' da nam '{ "id": 1, "name": "Ada" }'
```

## Deep Dive
JSON w Elm ma swoje korzenie w potrzebie typu bezpiecznego kodowania i dekodowania. Alternatywą jest używanie JavaScriptu po stronie serwera, ale to jest mniej bezpieczne. Elm używa specjalnych dekoderów, które gwarantują, że dane są dokładnie tym, czego oczekujemy, chroniąc przed błędami w czasie wykonania.

## See Also
- [Elm JSON.Decode documentation](https://package.elm-lang.org/packages/elm/json/latest/Json-Decode)
- [Elm JSON.Encode documentation](https://package.elm-lang.org/packages/elm/json/latest/Json-Encode)
- [Elm Guide on JSON](https://guide.elm-lang.org/effects/json.html)
