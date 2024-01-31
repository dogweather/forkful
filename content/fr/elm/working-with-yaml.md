---
title:                "Travailler avec YAML"
date:                  2024-01-19
simple_title:         "Travailler avec YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
Le YAML, c'est du texte pour des données. Les dev l'adorent pour sa simplicité, idéal pour la config ou des fichiers lourds en JSON.

## How to:
Elm n'a pas de lib standard pour YAML, mais on peut convertir du JSON en Elm. Utilisez `Json.Decode` pour parser un yaml converti.

```Elm
import Json.Decode as Decode
import Http

type alias User =
    { id : Int
    , name : String
    }

userDecoder : Decode.Decoder User
userDecoder =
    Decode.map2 User
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)

fetchUser : String -> Cmd Msg
fetchUser url =
    Http.get { url = url, decoder = userDecoder }
```

Si YAML est déjà converti en JSON...

```Elm
jsonString : String
jsonString =
    """
    { "id": 1, "name": "Alice" }
    """

parseResult : Result String User
parseResult =
    Decode.decodeString userDecoder jsonString
```

## Deep Dive
Elm est jeune : pas encore de lib YAML native. Autres langages en ont, comme PyYAML en Python. Si besoin, convertissez YAML à JSON et utilisez Elm.

## See Also
- Elm JSON guide: https://guide.elm-lang.org/effects/json.html
- YAML to JSON online: https://yamltojson.com
- PyYAML pour inspiration: https://pyyaml.org

(elm-yaml n'est pas encore là, mais gardez un œil ouvert!)
