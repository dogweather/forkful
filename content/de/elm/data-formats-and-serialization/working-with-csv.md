---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:14.836434-07:00
description: "Wie geht das: Elm bietet keine integrierte Unterst\xFCtzung f\xFCr das\
  \ Parsen oder Generieren von CSV; stattdessen werden oft Drittanbieter-Pakete wie\u2026"
lastmod: '2024-03-13T22:44:53.827919-06:00'
model: gpt-4-0125-preview
summary: "Elm bietet keine integrierte Unterst\xFCtzung f\xFCr das Parsen oder Generieren\
  \ von CSV; stattdessen werden oft Drittanbieter-Pakete wie `panosoft/elm-csv` verwendet."
title: Arbeiten mit CSV
weight: 37
---

## Wie geht das:
Elm bietet keine integrierte Unterstützung für das Parsen oder Generieren von CSV; stattdessen werden oft Drittanbieter-Pakete wie `panosoft/elm-csv` verwendet. Die untenstehenden Beispiele heben die grundlegende Nutzung dieser Bibliothek für das Parsen und Generieren von CSV hervor.

### CSV parsen
Zuerst müssen Sie das CSV-Paket zu Ihrem Elm-Projekt hinzufügen:

```bash
elm install panosoft/elm-csv
```

Dann können Sie einen CSV-String in eine Liste von Datensätzen parsen. Ein einfaches Beispiel:

```elm
import Csv

csvData : String
csvData =
    "name,age\nJohn Doe,30\nJane Smith,25"

parseResult : Result String (List (List String))
parseResult =
    Csv.parse csvData

-- Beispiel-Ausgabe: Ok [["name","age"],["John Doe","30"],["Jane Smith","25"]]
```

### CSV generieren
Um aus Elm-Daten einen CSV-String zu generieren, verwenden Sie die Funktion `Csv.encode`:

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

-- Beispiel-Ausgabe: "name,age\nJohn Doe,30\nJane Smith,25\n"
```

Dieser vereinfachte Ansatz ermöglicht es Ihnen, CSV-Funktionalitäten in Ihre Elm-Anwendungen zu integrieren und dabei die typsichere Umgebung für die Datenmanipulation und -austausch zu nutzen.
