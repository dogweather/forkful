---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:15.642754-07:00
description: "Comment faire : Elm ne dispose pas d'un support int\xE9gr\xE9 pour l'analyse\
  \ ou la g\xE9n\xE9ration de CSV ; \xE0 la place, des paquets tiers tels que `panosoft/elm-csv`\u2026"
lastmod: '2024-03-13T22:44:57.714370-06:00'
model: gpt-4-0125-preview
summary: "Elm ne dispose pas d'un support int\xE9gr\xE9 pour l'analyse ou la g\xE9\
  n\xE9ration de CSV ; \xE0 la place, des paquets tiers tels que `panosoft/elm-csv`\
  \ sont souvent utilis\xE9s."
title: Travailler avec CSV
weight: 37
---

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
