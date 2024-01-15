---
title:                "Travailler avec les fichiers csv"
html_title:           "Elm: Travailler avec les fichiers csv"
simple_title:         "Travailler avec les fichiers csv"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/working-with-csv.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous travaillez avec des données structurées, telles que des feuilles de calcul ou des bases de données, vous avez probablement eu affaire au format CSV. Ce format est largement utilisé pour stocker et échanger des données tabulaires en raison de sa simplicité et de sa compatibilité avec de nombreux logiciels. En utilisant Elm, vous pouvez facilement intégrer la manipulation de fichiers CSV dans vos projets et travailler avec ces données de manière structurée et efficace.

## Comment faire

Pour commencer à travailler avec des fichiers CSV en Elm, vous devez inclure le paquet "elm/parser" dans votre projet. Ce paquet contient des fonctions utiles pour analyser les données CSV et les transformer en structures de données utilisables.

Voici un exemple de code pour lire un fichier CSV et afficher son contenu dans la console :

```Elm
import Csv
import Delimited exposing (..)

-- Fonction pour convertir une chaîne CSV en une liste de listes de chaînes
parseCsv : String -> List (List String)
parseCsv input =
    input
        |> Delimited.fromString
        |> Result.map (Csv.parseWithParser Csv.csv)
        |> Result.withDefault []

-- Lire le fichier CSV et afficher son contenu
main : Program Never
main =
    File.contents "mon-fichier.csv"
        |> Task.perform (always []) parseCsv
        |> Task.map (List.map (\row -> row |> String.join ", " |> Debug.log "ligne CSV"))
```

En utilisant cette méthode, vous pouvez facilement lire un fichier CSV et travailler avec ses données dans votre application Elm.

## Plongée en profondeur

En plus de lire les données d'un fichier CSV, vous pouvez également utiliser les fonctions de transformation de données du paquet "elm/parser" pour manipuler les données et les enregistrer dans un nouveau format. Par exemple, vous pouvez trier les données par ordre alphabétique ou filtrer les lignes en fonction de certains critères.

De plus, vous pouvez également utiliser des bibliothèques externes telles que "NoRedInk/elm-csv" qui offrent des fonctionnalités supplémentaires pour travailler avec des données CSV, telles que la création de diagrammes ou de graphiques à partir de vos données.

## Voir aussi

- [Documentation officielle du paquet "elm/parser"](https://package.elm-lang.org/packages/elm/parser/latest/)
- [Documentation officielle du paquet "NoRedInk/elm-csv"](https://package.elm-lang.org/packages/NoRedInk/elm-csv/latest/)