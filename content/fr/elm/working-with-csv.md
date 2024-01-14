---
title:                "Elm: Travailler avec les fichiers csv"
simple_title:         "Travailler avec les fichiers csv"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/working-with-csv.md"
---

{{< edit_this_page >}}

## Pourquoi

CSV (Comma Separated Values) est un format de fichier couramment utilisé pour stocker des données tabulaires telles que les données financières, les listes de contacts, ou même des données de traduction. Travailler avec des fichiers CSV peut être très utile pour les développeurs, car cela leur permet de traiter et d'analyser facilement de grandes quantités de données tabulaires.

## Comment faire

Pour travailler avec des fichiers CSV en Elm, nous allons utiliser le package "csv" qui peut être trouvé sur le site de la bibliothèque Elm. Tout d'abord, il faut ajouter ce package à votre fichier Elm en utilisant la commande : `elm install elm-csv`. Ensuite, vous pouvez charger vos données CSV en utilisant la fonction `Csv.Decode.decodeString`.

```Elm
import Csv
import Csv.Decode as Decode

main =
    Csv.Decode.decodeString csvDecoder csvData

csvDecoder : Decode.Decoder (List (List String))
csvDecoder =
    let
        separator =
            Decode.succeed identity
    in
        Csv.Decode.fields separator
```

Dans cet exemple, nous utilisons la fonction `fields` pour séparer les données en colonnes et la fonction `succeed` pour ignorer le premier ligne qui contient les en-têtes de colonnes. Vous pouvez également utiliser d'autres fonctions telles que `field` pour décoder des colonnes spécifiques ou `row` pour décoder des lignes complètes.

Une fois que vous avez chargé vos données CSV, vous pouvez les manipuler en utilisant les fonctions de la bibliothèque Elm telles que `List.map` et `List.filter`. Ensuite, vous pouvez afficher les données résultantes en utilisant la fonction `text` du package `elm-lang/html`.

```Elm
view : List (List String) -> Html msg
view csvData =
    div [] (List.map rowToHtml csvData)

rowToHtml : List String -> Html msg
rowToHtml row =
    div []
        (List.map cellToHtml row)

cellToHtml : String -> Html msg
cellToHtml cell =
    text cell
```

## Plongée en profondeur

Il est important de noter que le package "csv" n'est pas conçu pour gérer des données CSV avec des en-têtes de colonnes multiples ou des valeurs vides. Dans ces cas, vous pouvez utiliser le package "elm-csv-decode" qui offre une fonctionnalité plus robuste pour traiter toutes sortes de données CSV.

De plus, si vous souhaitez générer des fichiers CSV à partir de données Elm, vous pouvez utiliser le package "jxxcarlson/elm-csv" qui inclut des fonctions pour convertir des données Elm en format CSV.

## Voir aussi

- La bibliothèque Elm : https://package.elm-lang.org/
- Package "csv" : https://package.elm-lang.org/packages/ozmat/elm-csv/latest/
- Package "elm-csv-decode" : https://package.elm-lang.org/packages/NoRedInk/elm-csv-decode/latest/
- Package "jxxcarlson/elm-csv" : https://package.elm-lang.org/packages/jxxcarlson/elm-csv/latest/