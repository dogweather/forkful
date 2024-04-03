---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:31.003091-07:00
description: "Att arbeta med CSV (Comma Separated Values) involverar tolkning och\
  \ generering av filer som lagrar tabul\xE4r data i ett enkelt, klartextformat. Detta\u2026"
lastmod: '2024-03-13T22:44:37.851959-06:00'
model: gpt-4-0125-preview
summary: "Att arbeta med CSV (Comma Separated Values) involverar tolkning och generering\
  \ av filer som lagrar tabul\xE4r data i ett enkelt, klartextformat."
title: Arbeta med CSV
weight: 37
---

## Hur man gör:
Elm har inte inbyggt stöd för tolkning eller generering av CSV; istället används ofta tredjepartspaket såsom `panosoft/elm-csv`. Nedanstående exempel lyfter fram grundläggande användning av detta bibliotek för tolkning och generering av CSV.

### Tolka CSV
Först behöver du lägga till CSV-paketet till ditt Elm-projekt:

```bash
elm install panosoft/elm-csv
```

Sedan kan du tolka en CSV-sträng till en lista av poster. Ett enkelt exempel:

```elm
import Csv

csvData : String
csvData =
    "name,age\nJohn Doe,30\nJane Smith,25"

parseResult : Result String (List (List String))
parseResult =
    Csv.parse csvData

-- Exempel på utdata: Ok [["name","age"],["John Doe","30"],["Jane Smith","25"]]
```

### Generera CSV
För att generera en CSV-sträng från Elm-data, använd funktionen `Csv.encode`:

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

-- Exempel på utdata: "name,age\nJohn Doe,30\nJane Smith,25\n"
```

Detta förenklade tillvägagångssätt gör det möjligt för dig att integrera CSV-funktionalitet inom dina Elm-applikationer, med fördelen av en typsäker miljö för datahantering och utbyte.
