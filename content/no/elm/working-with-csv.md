---
title:                "Å jobbe med csv"
html_title:           "Elm: Å jobbe med csv"
simple_title:         "Å jobbe med csv"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/working-with-csv.md"
---

{{< edit_this_page >}}

Hva & Hvorfor?
CSV (Comma-Separated Values) er en filformat som brukes til å lagre og overføre strukturerte data, spesielt i databaser og regneark. Det er populært blant programmerere fordi det er enkelt å lese og håndtere, og det er også kompatibelt med de fleste språk og verktøy.

Hvordan:
Elm har et innebygd bibliotek, kalt "csv", som tillater å arbeide med og behandle CSV-data. For å bruke det, trenger du bare å importere det i koden din med `import Csv`. Deretter kan du bruke funksjoner som `parse` for å konvertere en CSV-streng til en liste av lister, og `encode` for å lage en CSV-streng fra en liste av lister.

```Elm
import Csv

myCsv = "name,age,city
John,25,Oslo
Lisa,32,Bergen
Eric,45,Trondheim"

parsed = Csv.parse myCsv
-- [["name", "age", "city"], ["John", "25", "Oslo"], ["Lisa", "32", "Bergen"], ["Eric", "45", "Trondheim"]]

encoded = Csv.encode parsed
-- "name,age,city
-- John,25,Oslo
-- Lisa,32,Bergen
-- Eric,45,Trondheim"
```

Dypdykk:
CSV ble opprinnelig utviklet på 1970-tallet som et enkelt format for å lagre og utveksle data mellom databaser og regneark. Siden da har det blitt utbredt og brukes nå også i web-utvikling, for eksempel for å importere og eksportere data fra en database til en nettside.

Alternativet til å bruke CSV i Elm er å jobbe direkte med JSON-data. Dette kan være mer effektivt for store og komplekse datasett, men for enklere data og mindre prosjekter, kan CSV være mer praktisk og enkelt å håndtere.

Se også:
- Offisiell dokumentasjon for Elm CSV-biblioteket: https://package.elm-lang.org/packages/elm/csv/latest/
- En grundig introduksjon til CSV-formatet: https://www.webopedia.com/TERM/C/CSV.html
- En sammenligning mellom CSV og JSON: https://www.geeksforgeeks.org/difference-between-csv-and-json/