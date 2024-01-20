---
title:                "Arbeta med csv"
html_title:           "Arduino: Arbeta med csv"
simple_title:         "Arbeta med csv"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
CSV står för "Comma-Separated Values". Det är ett enkelt filformat som används för att lagra tabulär data, som i kalkylblad eller databaser. Programmerare använder CSV för att enkelt importera och exportera data från olika applikationer.

## How to:
Elm har inget inbyggt bibliotek för CSV-hantering, så vi använder en extern paket, som `elm-csv`. Installera det med `elm install panthershark/elm-csv` och kolla på koden nedan:

```Elm
import Csv

decodeCsv : String -> List (List String)
decodeCsv data =
    case Csv.decode data of
        Ok rows ->
            rows

        Err errorMessage ->
            []

sampleCsv : String
sampleCsv =
    "name,age\nAlice,30\nBob,25"

-- Använd `decodeCsv` och skriv ut resultatet
main =
    decodeCsv sampleCsv |> toString |> text
```

Output i Elm's `main` blir: `[["name", "age"], ["Alice", "30"], ["Bob", "25"]]`

## Deep Dive
CSV skapades på 1970-talet och är fortfarande populärt på grund av sin enkelhet. Alternativ till CSV inkluderar JSON och XML, men de är inte lika lättlästa för människor. När man använder `elm-csv`, hanteras CSV-strängen genom att dela upp den i rader och sedan kolumner. Detaljer som att hantera specialtecken och radavslutningar sköts automatiskt.

## See Also
- Elm officiell guide: [The Official Elm Guide](https://guide.elm-lang.org/)
- CSV på Wikipedia: [Comma-Separated Values on Wikipedia](https://en.wikipedia.org/wiki/Comma-separated_values)