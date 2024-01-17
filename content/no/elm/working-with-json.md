---
title:                "Arbeid med json"
html_title:           "Elm: Arbeid med json"
simple_title:         "Arbeid med json"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/working-with-json.md"
---

{{< edit_this_page >}}

Hva & Hvorfor?
Å arbeide med JSON er å håndtere data i et lett og leselig format. Programmerere bruker det for å effektivt utveksle og lagre data på tvers av ulike språk og plattformer.

Hvordan:
Elm spiller godt sammen med JSON og gjør det enkelt å både konvertere data til og fra JSON-formatet, samt å pakke den inn i type-sikre Elm-strukturer.
```elm
-- Pakk data inn i en Json-struktur
type alias User =
    { firstName : String
    , lastName : String
    }

userToJson : User -> Json.Encode.Value
userToJson user =
    Json.Encode.object
        [ ( "firstName", Json.Encode.string user.firstName )
        , ( "lastName", Json.Encode.string user.lastName )
        ]

-- Konverter data fra Json til en Elm-struktur
jsonToUser : Json.Decoder User
jsonToUser =
    Json.Decode.map2 User
        (Json.Decode.field "firstName" Json.Decode.string)
        (Json.Decode.field "lastName" Json.Decode.string)
```

Dypdykk:
JSON ble først utviklet av Douglas Crockford på slutten av 90-tallet og har siden blitt et av de mest brukte formatene for å lagre og utveksle data. Alternativer til JSON inkluderer XML og YAML, men JSON er foretrukket på grunn av sitt enkle og kompakte format. I Elm bruker man kirreringer og avkoding for å arbeide med JSON.

Se også:
Offisiell Elm dokumentasjon for JSON: https://package.elm-lang.org/packages/elm/json/latest/