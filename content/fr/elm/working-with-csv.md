---
title:                "Manipulation des fichiers CSV"
date:                  2024-01-19
html_title:           "Bash: Manipulation des fichiers CSV"
simple_title:         "Manipulation des fichiers CSV"

category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
On jongle souvent avec des fichiers CSV - c'est du texte avec des valeurs séparées par des virgules. En Elm, on s'en sert pour échanger des données facilement entre systèmes et pour le reporting.

## How to:
En Elm, il n'y a pas de bibliothèque CSV intégrée, donc on peut utiliser `elm-csv` ou gérer les strings manuellement.

```Elm
import Csv

-- Parsing CSV text into a list of records
let
    csvText = "name,age\nAlice,30\nBob,25"
in
Csv.parse csvText
-- Résultat: Ok [["name","age"], ["Alice","30"], ["Bob","25"]]
```

On convertit les lignes en records avec un peu de logique supplémentaire:

```Elm
import Csv

type alias User =
    { name : String
    , age : Int
    }

parseUser : List String -> Result String User
parseUser [name, ageString] =
    case String.toInt ageString of
        Just age ->
            Ok { name = name, age = age }

        Nothing ->
            Err "Cannot convert the age to an integer"

parseUser _ =
    Err "Invalid user data"

parseCsv : String -> Result String (List User)
parseCsv csvText =
    let
        parsedCsv = Csv.parse csvText
    in
    parsedCsv
        |> Result.map (List.map parseUser)
        |> Result.map (List.filterMap identity)
        |> Result.mapError (\_ -> "Failed to parse CSV")

```

## Deep Dive
CSV, créé dans les années 70, était initialement destiné à simplifier les transferts de données entre programmes. Aujourd'hui, il existe des bibliothèques CSV dans la plupart des langages de programmation, mais en Elm, on fait plus souvent appel à un module externe (comme `elm-csv`) pour traiter ces données. Bien que JSON soit devenu le format de choix pour les API web, CSV reste populaire pour les imports/exports et l'analyse de données.

## See Also
- [Elm Guide pour JSON](https://guide.elm-lang.org/interop/json.html)
- [RFC4180 pour les spécifications CSV standards](https://tools.ietf.org/html/rfc4180)
