---
aliases:
- /fr/elm/working-with-csv/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:15.642754-07:00
description: "Travailler avec du CSV (Valeurs S\xE9par\xE9es par des Virgules) implique\
  \ d'analyser et de g\xE9n\xE9rer des fichiers qui stockent des donn\xE9es tabulaires\
  \ dans un\u2026"
lastmod: 2024-02-18 23:09:08.759080
model: gpt-4-0125-preview
summary: "Travailler avec du CSV (Valeurs S\xE9par\xE9es par des Virgules) implique\
  \ d'analyser et de g\xE9n\xE9rer des fichiers qui stockent des donn\xE9es tabulaires\
  \ dans un\u2026"
title: Travailler avec CSV
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Travailler avec du CSV (Valeurs Séparées par des Virgules) implique d'analyser et de générer des fichiers qui stockent des données tabulaires dans un format texte simple. Cette pratique est courante chez les programmeurs pour permettre un échange de données facile entre différentes applications ou pour traiter de grands ensembles de données de manière efficace et sûre en termes de type dans Elm.

## Comment faire :

Elm ne dispose pas d'un support intégré pour l'analyse ou la génération de CSV ; à la place, des paquets tiers tels que `panosoft/elm-csv` sont souvent utilisés. Les exemples ci-dessous mettent en lumière l'utilisation de base de cette bibliothèque pour l'analyse et la génération de CSV.

### Analyser du CSV

Tout d'abord, vous devez ajouter le paquet CSV à votre projet Elm :

```bash
elm install panosoft/elm-csv
```

Ensuite, vous pouvez analyser une chaîne CSV en une liste d'enregistrements. Un exemple simple :

```elm
import Csv

csvData : String
csvData =
    "name,age\nJohn Doe,30\nJane Smith,25"

parseResult : Result String (List (List String))
parseResult =
    Csv.parse csvData

-- Sortie d'exemple : Ok [["name","age"],["John Doe","30"],["Jane Smith","25"]]
```

### Générer du CSV

Pour générer une chaîne CSV à partir des données Elm, utilisez la fonction `Csv.encode` :

```elm
import Csv

records : List (List String)
records =
    [ ["name", "age"]
    , ["John Doe", "30"]
    , ["Jane Smith", "25"]
    ]

csvOutput : String
csvOutput =
    Csv.encode records

-- Sortie d'exemple : "name,age\nJohn Doe,30\nJane Smith,25\n"
```

Cette approche simpliste vous permet d'intégrer des fonctionnalités CSV au sein de vos applications Elm, en tirant parti de l'environnement sûr en termes de type pour la manipulation et l'échange de données.
