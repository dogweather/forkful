---
title:                "Arbeiten mit CSV"
date:                  2024-02-03T19:19:14.836434-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeiten mit CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Die Arbeit mit CSV (Comma Separated Values) umfasst das Parsen und Generieren von Dateien, die tabellarische Daten in einem einfachen, Klartext-Format speichern. Dies wird häufig von Programmierern praktiziert, um einen einfachen Datenaustausch zwischen verschiedenen Anwendungen zu ermöglichen oder um große Datensätze effizient und typsicher in Elm zu verarbeiten.

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
