---
title:                "Робота з JSON"
html_title:           "Arduino: Робота з JSON"
simple_title:         "Робота з JSON"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/working-with-json.md"
---

{{< edit_this_page >}}

## Що & Чому?
Робота з JSON (JavaScript Object Notation) - це обмін даними між сервером і клієнтом. Програмісти використовують JSON для роботи з даними через його легку читаємість і широку підтримку у веб-технологіях.

## Як це зробити:
```Elm
import Html exposing (text)
import Json.Decode exposing (decodeString)
import Json.Encode exposing (object, string)

type alias User =
    { name : String
    , email : String
    }

userDecoder : Json.Decode.Decoder User
userDecoder =
    Json.Decode.map2 User
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "email" Json.Decode.string)

userEncoder : User -> Json.Encode.Value
userEncoder user =
    object
        [ ( "name", Json.Encode.string user.name )
        , ( "email", Json.Encode.string user.email )
        ]

main =
    let
        userData =
            """
            {"name":"Oleksiy","email":"oleksiy@example.com"}
            """

        decoded = decodeString userDecoder userData
    in
    case decoded of
        Ok user ->
            text (user.name ++ " " ++ user.email)

        Err err ->
            text ("Could not decode user: " ++ toString err)

-- Вивід:
-- Oleksiy oleksiy@example.com
```

## Поглиблений Розділ
JSON виник як простіший альтернатива до XML і швидко став стандартом для веб-додатків. В Elm JSON-декодер і енкодер - це містки між строго типізованим Elm кодом і гнучким форматом JSON. Ви можете використовувати пакет `elm/json` для роботи з JSON, але, на відміну від багатьох мов програмування, Elm потребує явного опису структури даних.

## Дивіться також:
- Офіційна документація JSON в Elm: https://package.elm-lang.org/packages/elm/json/latest/
- Elm Guide - глава про роботу з JSON: https://guide.elm-lang.org/effects/json.html
- Детальний туторіал про custom types і JSON: https://thoughtbot.com/blog/decoding-json-structures-with-elm