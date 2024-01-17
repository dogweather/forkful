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

## Vad & Varför?
CSV står för "comma-separated values" och är ett format för att lagra och hantera tabellinformation i textform. Det är vanligtvis användbart när man behöver bearbeta stora mängder data, särskilt när det gäller att importera eller exportera tabellinformation från olika program och verktyg. CSV är också lättläst för både människor och datorer, vilket gör det till ett populärt val för datalagring.

## Hur man gör:
Gleam har inbyggda funktioner för att läsa och skriva CSV-filer, vilket gör det enkelt att arbeta med detta format. Här är ett exempel på hur man läser en CSV-fil och skriver ut den till konsolen:

```
let rows = file::csv::read("data.csv")

do
  rows
  |> List.iter(fn(row) -> 
    row
    |> List.map(fn(cell) -> 
      cell
      |> String.join(", ")
    )
    |> String.join(" | ")
    |> io::println
  )
```

Detta kodexempel använder funktionen `file::csv::read` för att läsa in en CSV-fil och lagrar resultaten i en lista. Sedan använder den sedan den inbyggda höjnivåfunktionen `List.iter` för att iterera över varje rad i filen och skriva ut den till konsolen i ett läsbart format.

## Djupdykning:
CSV-formatet uppfanns på 1970-talet och har sedan dess blivit en standard för datahantering, särskilt inom affärsvärlden. Det finns också alternativ till CSV, som till exempel JSON och XML, men CSV behåller sin popularitet på grund av dess enkelhet och kompatibilitet med olika program.

När man arbetar med CSV i Gleam, är det viktigt att se till att både din kod och CSV-filen är korrekt formaterade. Om kolumnerna i din CSV-fil inte stämmer överens med de datatyper du förväntar dig, kan din kod ge oväntade resultat eller felmeddelanden. Se till att du också kontrollerar om filer har några speciella tecken som kan förstöra parsningen.

## Se även:
- Officiell dokumentation för CSV-funktioner i Gleam: https://gleam.run/lib/file.csv.html
- En introduktion till CSV-formatet: https://en.wikipedia.org/wiki/Comma-separated_values