---
title:                "Å arbeide med csv"
html_title:           "Gleam: Å arbeide med csv"
simple_title:         "Å arbeide med csv"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/working-with-csv.md"
---

{{< edit_this_page >}}

## Hvorfor

CSV (Comma-Separated Values) er en utbredt filtype for å lagre og dele data i et lettleselig format. Det er ofte brukt i ulike bransjer som finans, forskning og marked. Å kunne håndtere CSV-filer er en svært nyttig ferdighet for forskere, dataanalytikere og utviklere.

## Hvordan

For å arbeide med CSV-filer i Gleam, er det først nødvendig å importere standardbiblioteket `csv`. Deretter kan du bruke funksjoner som `from_file` for å lese data fra en CSV-fil og `to_file` for å skrive data til en CSV-fil. La oss se på et eksempel:

```Gleam
import csv

// Les inn data fra en CSV-fil
let data = csv.from_file("example.csv")

// Skriver data til en CSV-fil
let headers = ["Navn", "Alder", "Stilling"]
let ansatte = [
  ["Maria", "32", "Markedsfører"],
  ["Jonas", "24", "Utvikler"],
  ["Sofia", "27", "Prosjektleder"]
]
csv.to_file("ansatte.csv", headers, ansatte)

// Output: CSV-filen "ansatte.csv" vil se slik ut:
//
// Navn,Alder,Stilling
// Maria,32,Markedsfører
// Jonas,24,Utvikler
// Sofia,27,Prosjektleder
```

## Dypdykk

Det er ofte nødvendig å kunne manipulere og bearbeide data fra CSV-filer. Med Gleam kan du bruke funksjoner som `map` og `filter` for å transformere dataene slik du ønsker. La oss se på et dypere eksempel:

```Gleam
import csv

// Les inn data fra en CSV-fil
let data = csv.from_file("tall.csv")

// Mapper data til et nytt format
let res = data
  |> List.map((row) =>
    {
      "Tall": row.get_column("Tall"),
      "Doblet": Int.parse(row.get_column("Tall")) * 2
    }
  )
  // Fjerner alle tall som er mindre enn 10
  |> List.filter((x) => x.Tall > 10)

// Skriver resultatet til en ny CSV-fil
let headers = ["Tall", "Doblet"]
csv.to_file("dobla-tall.csv", headers, res)

// Output: CSV-filen "dobla-tall.csv" vil se slik ut:
//
// Tall,Doblet
// 15,30
// 27,54
// 33,66
```

## Se også

- CSV biblioteksdokumentasjon: https://gleam.run/modules/csv/latest/
- Gleam offisiell nettside: https://gleam.run/
- Mer om CSV-filer: https://en.wikipedia.org/wiki/Comma-separated_values