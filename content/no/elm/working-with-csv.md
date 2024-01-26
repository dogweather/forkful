---
title:                "Arbeid med CSV"
html_title:           "Bash: Arbeid med CSV"
simple_title:         "Arbeid med CSV"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/working-with-csv.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Arbeid med CSV (Comma-Separated Values) involverer behandling av data som er skilt med komma, ofte brukt for å lagre og dele store datamengder. Programmerere bruker det fordi det er en enkel og universell format for utveksling av data mellom ulike systemer.

## Hvordan:
```Elm
import Csv

csvData : String
csvData =
    "name,age\nAlice,30\nBob,24"

parseResult : Result Csv.Error (List (List String))
parseResult =
    Csv.decode csvData

-- Forventet resultat:
-- Ok [ ["name", "age"], ["Alice", "30"], ["Bob", "24"] ]
```

## Dypdykk
CSV-formatet har sin røtter fra tidlig datautveksling, brukt for sin enkelhet. Alternativer som JSON eller XML tilbyr mer struktur, men CSV er fortsatt populær for enkelhet og lesbarhet. Når du jobber med CSV i Elm, er det essensielt å ha en god parser som kan håndtere forskjellige kanttilfeller og inkonsistenser som ofte oppstår i CSV-filer.

## Se Også
- Elm CSV pakke dokumentasjon: https://package.elm-lang.org/packages/elm/csv/latest/
- Elm guide for å jobbe med eksterne data: https://guide.elm-lang.org/effects/json.html
- En artikkel om beste praksiser for CSV-formatet: https://www.ietf.org/rfc/rfc4180.txt
