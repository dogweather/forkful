---
title:                "Manipulation de JSON"
date:                  2024-01-19
simple_title:         "Manipulation de JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
"Quoi et Pourquoi ?"

Le JSON est un format de données; on l'utilise pour échanger des infos entre le client et le serveur. C'est léger et facile à comprendre, donc on l'adore dans les applis web.

## How to:
"Comment faire :"

Commence par ajouter `elm/json` à ton projet :
```
elm install elm/json
```

Decode du JSON :
```Elm
import Json.Decode exposing (string, int, decodeValue)

jsonString = """
{ "name": "Bob", "age": 30 }
"""

type alias User =
    { name : String
    , age : Int
    }

userDecoder =
    Json.Decode.map2 User
        (Json.Decode.field "name" string)
        (Json.Decode.field "age" int)

case decodeValue userDecoder (Json.Decode.string jsonString) of
    Ok user -> 
        -- utilises `user` ici
    Err error ->
        -- gère les erreurs ici
```

Encode en JSON :
```Elm
import Json.Encode exposing (object, string, int)

userEncoder : User -> Json.Encode.Value
userEncoder user =
    object
        [ ( "name", string user.name )
        , ( "age", int user.age )
        ]

encodedUser = userEncoder { name = "Bob", age = 30 }
-- `encodedUser` est maintenant un JSON prêt à être envoyé
```

## Deep Dive
"Plongée en Profondeur"

JSON, c'est JavaScript Object Notation, né dans les années 2000 pour les JavaScript apps. Maintenant, c'est standard partout. En Elm, on manipule du JSON avec des 'decoders' et 'encoders' parce que Elm est type-safe; ça évite des surprises. Elm n'a pas de null ou undefined, donc le JSON doit être converti proprement. Les alternatives comme XML sont là, mais JSON est roi pour sa simplicité.

## See Also
"Voir Aussi"

- Officiel Elm Guide sur JSON: [https://guide.elm-lang.org/effects/json.html](https://guide.elm-lang.org/effects/json.html)
- `elm/json` package: [https://package.elm-lang.org/packages/elm/json/latest/](https://package.elm-lang.org/packages/elm/json/latest/)
- JSON: [http://json.org/](http://json.org/)

C'est tout. A toi de coder !
