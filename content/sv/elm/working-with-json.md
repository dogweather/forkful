---
title:                "Arbeta med JSON"
date:                  2024-01-19
html_title:           "Arduino: Arbeta med JSON"
simple_title:         "Arbeta med JSON"

category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/working-with-json.md"
---

{{< edit_this_page >}}

## Vad & Varför?
JSON, JavaScript Object Notation, är ett dataformat används för att lagra och transportera data. Programmerare använder det för att enkelt utbyta data mellan server och webbapplikationer.

## Hur gör man:
I Elm använder man `Json.Decode` och `Json.Encode` för att hantera JSON. Här är ett exempel på dekodning från JSON till Elm och kodning från Elm till JSON.

```Elm
import Json.Decode exposing (field, int, string, decodeValue)
import Json.Encode exposing (object, string, int)

-- JSON till Elm
type alias User =
    { id : Int
    , name : String
    }

userDecoder : Json.Decode.Decoder User
userDecoder =
    Json.Decode.map2 User
        (field "id" int)
        (field "name" string)

jsonString : String
jsonString =
    "{\"id\": 123, \"name\": \"Anna\"}"

decodedUserResult : Result String User
decodedUserResult =
    decodeValue userDecoder (Json.Decode.string jsonString)

-- Elm till JSON
userEncoder : User -> Json.Encode.Value
userEncoder user =
    object
        [ ("id", int user.id)
        , ("name", string user.name)
        ]

encodedUser : Json.Encode.Value
encodedUser = 
    userEncoder { id = 123, name = "Anna" }
```

Sample output från dekodningen skulle vara:

```Elm
Ok { id = 123, name = "Anna" }
```

## Fördjupning
JSON introducerades 2001 och är baserat på JavaScript-syntaxen. Det har blivit det primära formatet för API-kommunikation. Alternativ till JSON inkluderar XML och YAML, men JSON vinner på grund av sin enkelhet. I Elm, hanterar decoders och encoders konverteringen och ser till att datatypen matchar, vilket minimerar runtime-fel. 

## Se även
- Elm JSON guide: https://guide.elm-lang.org/effects/json.html
- JSON.org för att lära dig mer om JSON-formatet: http://json.org/
- `elm/json` paketet på Elm package website: https://package.elm-lang.org/packages/elm/json/latest/
