---
title:                "Arbeta med csv"
html_title:           "Gleam: Arbeta med csv"
simple_title:         "Arbeta med csv"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/working-with-csv.md"
---

{{< edit_this_page >}}

## Varför
CSV (Comma-Separated Values) är en vanlig format för att lagra och dela tabulära data. Det är ett användbart verktyg för programmerare att arbeta med eftersom det är lätt att läsa och hantera data från CSV-filer.

## Hur man gör
Att arbeta med CSV i Gleam är enkelt och smidigt. Först behöver vi importera paketet `"csv"` för att få tillgång till dess funktioner. Sedan kan vi använda funktionen `Csv.read` för att läsa in en CSV-fil som en lista av rader och kolumner.

```Gleam
use gleam/csv

let csv = "namn,ålder,land
Lisa,25,Sverige
Eva,30,Norge"

let data = Csv.read(csv)

// data blir: [ ["namn", "ålder", "land"], ["Lisa", "25", "Sverige"], ["Eva", "30", "Norge"] ]
```

Om du vill ändra på formatet av en CSV fil, kan du använda funktionen `Csv.format` för att omvandla listan av rader och kolumner till en sträng i CSV-format.

```Gleam
use gleam/csv

let data = [
  ["namn", "ålder", "land"],
  ["Lisa", "25", "Sverige"],
  ["Eva", "30", "Norge"]
]

let csv = Csv.format(data)

// data blir: "namn,ålder,land
// Lisa,25,Sverige
// Eva,30,Norge"
```

För att skriva ut en CSV-fil till en fil kan vi använda funktionen `Csv.write` tillsammans med en filhandtag.

```Gleam
use gleam/csv
use gleam/io

let data = [
  ["namn", "ålder", "land"],
  ["Lisa", "25", "Sverige"],
  ["Eva", "30", "Norge"]
]

let csv = Csv.format(data)

let file = File.create("minfil.csv", .write)
Csv.write(file, csv)
```

## Djupdykning
Det finns många funktioner för att hantera CSV i Gleam, som till exempel `Csv.row` för att läsa in en specifik rad, `Csv.column` för att läsa in en specifik kolumn och `Csv.decode` för att omvandla data från CSV-filen till specifika typer.

Det är också möjligt att konfigurera `Csv.read` för att hantera specialfall, till exempel olika skiljetecken eller escape-tecken.

## Se även
- [Officiell dokumentation för gleam/csv paketet](https://gleam.run/packages/gleam/csv/latest/)
- [GitHub-repositorium för gleam/csv paketet](https://github.com/gleam-lang/csv)