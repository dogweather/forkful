---
title:                "Elm: Å jobbe med csv"
simple_title:         "Å jobbe med csv"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/working-with-csv.md"
---

{{< edit_this_page >}}

## Hvorfor

Å jobbe med CSV-filer kan være nyttig for de som trenger å håndtere store mengder data på en organisert og strukturert måte. Med Elm kan man enkelt lese og skrive CSV-filer ved hjelp av innebygde funksjoner og biblioteker.

## Hvordan gjøre det

For å lese en CSV-fil i Elm, kan man bruke funksjonen `Csv.Decode.decode`. Denne funksjonen tar inn en streng med CSV-data og returnerer en liste av lister som representerer hver rad og kolonne i filen. For eksempel:

```
Elm:

import Csv.Decode exposing (decode)

csvData = """
name,age,gender
John,25,male
Emily,30,female
"""

decode csvData --> 
Ok [["name", "age", "gender"], ["John", "25", "male"], ["Emily", "30", "female"]]
```

For å skrive ut en CSV-fil i Elm, kan man bruke biblioteket `Csv.Encode`. Ved å konvertere datastrukturen til en liste av lister til en `Csv.Encode.Value`, kan man deretter bruke funksjonen `Csv.Encode.encode` for å få en streng med CSV-data. For eksempel:

```
Elm:

import Csv.Encode exposing (encode)

csvData = [["name", "age", "gender"], ["John", "25", "male"], ["Emily", "30", "female"]]

encode csvData --> 
Ok "name,age,gender\r\nJohn,25,male\r\nEmily,30,female"
```

## Dypdykk

Ved å bruke elm-community-modulen `elm-csv`, er det enkelt å manipulere og behandle CSV-data i Elm. Dette biblioteket tilbyr funksjoner som å filtrere, sortere og gruppeere data basert på ulike kriterier.

Det er også mulig å lage egne konfigureringer for å håndtere CSV-filer med avvikende strukturer eller spesielle tegnsett. Dette kan gjøres ved å bruke funksjoner som `Csv.Decode.customDecoder` og `Csv.Encode.customEncoder`.

## Se også

- [elm-csv pakken på Elm pakkesentralen] (https://package.elm-lang.org/packages/elm-community/elm-csv/latest/)
- [Offisiell Elm dokumentasjon om CSV] (https://guide.elm-lang.org/interop/csv.html)
- [Eksempel på Elm kode for å arbeide med CSV] (https://ellie-app.com/84ZP6qpT9zja1)