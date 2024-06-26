---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:27.957010-07:00
description: "Hvordan: Elm har ikke innebygd st\xF8tte for parsing eller generering\
  \ av CSV; i stedet brukes ofte tredjepartspakker som `panosoft/elm-csv`. Nedenfor\u2026"
lastmod: '2024-03-13T22:44:40.730703-06:00'
model: gpt-4-0125-preview
summary: "Elm har ikke innebygd st\xF8tte for parsing eller generering av CSV; i stedet\
  \ brukes ofte tredjepartspakker som `panosoft/elm-csv`."
title: Arbeide med CSV
weight: 37
---

## Hvordan:
Elm har ikke innebygd støtte for parsing eller generering av CSV; i stedet brukes ofte tredjepartspakker som `panosoft/elm-csv`. Nedenfor eksemplene fremhever den grunnleggende bruken av dette biblioteket for parsing og generering av CSV.

### Parse CSV
Først må du legge til CSV-pakken til ditt Elm-prosjekt:

```bash
elm install panosoft/elm-csv
```

Deretter kan du parse en CSV-streng til en liste av poster. Et enkelt eksempel:

```elm
import Csv

csvData : String
csvData =
    "name,age\nJohn Doe,30\nJane Smith,25"

parseResult : Result String (List (List String))
parseResult =
    Csv.parse csvData

-- Eksempel på utdata: Ok [["name","age"],["John Doe","30"],["Jane Smith","25"]]
```

### Generere CSV
For å generere en CSV-streng fra Elm-data, bruk `Csv.encode` funksjonen:

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

-- Eksempel på utdata: "name,age\nJohn Doe,30\nJane Smith,25\n"
```

Denne enkle tilnærmingen gjør det mulig for deg å integrere CSV-funksjonalitetene innenfor dine Elm-applikasjoner, og utnytte det typesikre miljøet for datamanipulering og -utveksling.
