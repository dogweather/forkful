---
title:                "Arbeid med CSV"
html_title:           "Bash: Arbeid med CSV"
simple_title:         "Arbeid med CSV"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
CSV-håndtering betyr å lese og skrive data i "comma-separated values" format. Programmerere bruker det fordi det er enkelt og universelt - utmerket for datautveksling mellom ulike systemer.

## How to:
For å jobbe med CSV i Gleam, antar vi at en CSV-bibliotek funksjonalitet eksisterer, da Gleam er et nyere språk og standardbiblioteket kan være begrenset.

```gleam
import csv_lib.{parse_csv, write_csv}

// Parsing CSV-data til en liste med poster
fn main() {
  let csv_data = "name,age\nAlice,30\nBob,25"
  let records = parse_csv(csv_data)
  case records {
    Ok(rows) -> rows
    Error(e) -> []
  }
}

// Skriv ut til konsollen for å verifisere
let output = main()
io.println(output)
```

Forventet output:
```
[["name", "age"], ["Alice", "30"], ["Bob", "25"]]
```

Nå for å skrive data:

```gleam
// Lager en liste med poster og skriver det til CSV-format
fn write_sample_csv(data: List(List(String))) {
  let csv_string = write_csv(data)
  csv_string
}

fn main() {
  let data = [["name", "age"], ["Alice", "30"], ["Bob", "25"]]
  let csv_data = write_sample_csv(data)
  csv_data
}

// Skriv ut til konsollen for å verifisere
let csv_output = main()
io.println(csv_output)
```

Forventet output:
```
name,age
Alice,30
Bob,25
```

## Deep Dive
CSV står for "Comma-Separated Values" og ble populært på 1970-tallet for databasemanagement systemer. Alternativer til CSV inkluderer JSON, XML og YAML. CSV er fortsatt relevant på grunn av sin enkelhet og brede støtte, men kan være problematisk med komplekse datastrukturer. Implementasjoner varierer, men som regel inneholder en CSV-fil data rekorder med felter separert av komma og poster separert av linjeskift.

## See Also
- CSV standard: [https://tools.ietf.org/html/rfc4180](https://tools.ietf.org/html/rfc4180)
- Gleam CSV-relaterte biblioteker (så snart de blir tilgjengelig)
- Sammenligning av dataformater: [https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats](https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats)
