---
title:                "Swift: Å jobbe med csv"
simple_title:         "Å jobbe med csv"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/working-with-csv.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvorfor vil noen jobbe med CSV-filer? CSV er en av de mest populære filformatene for lagring av tabellbasert data. Det er en effektiv måte å organisere og lagre store mengder data på, og det er også kompatibelt med mange programmer og plattformer.

## Hvordan
For å jobbe med CSV-filer i Swift, må du først importere biblioteket "Foundation". Deretter kan du bruke funksjoner som "contentsOfURL" for å lese dataene i filen og "componentsSeparatedByString" for å dele dataene inn i riktig format. Her er et eksempel på hvordan du kan lese data fra en CSV-fil og legge den i en array:

```Swift
let csvFile = try String(contentsOf: URL(fileURLWithPath: "file.csv"), encoding: String.Encoding.utf8)
let data = csvFile.components(separatedBy: ",")
```

Etter å ha delt dataene i riktig format, kan du jobbe med dem som du vil, for eksempel ved å bruke løkker og betingelser for å filtrere og manipulere dataene. Når du er ferdig med å jobbe med dataene, kan du eksportere dem tilbake til en CSV-fil ved hjelp av funksjoner som "joined" og "data:usingEncoding".

## Deep Dive
En viktig ting å huske på når du jobber med CSV-filer er å håndtere eventuelle spesialtegn eller linjeskift som kan føre til problemer med dataene. For å unngå dette kan du bruke funksjoner som "trimmingCharacters(in:)" og "replacingOccurrences(of:with:)" for å rengjøre dataene dine før du behandler dem.

Det er også viktig å være oppmerksom på at CSV-filer kan ha en annen struktur og formattering, avhengig av hvilket program eller plattform de ble opprettet på. Så det kan være lurt å undersøke og tilpasse koden din for å håndtere ulike typer CSV-filer.

## Se også
- [Apple Docs - Foundation Framework](https://developer.apple.com/documentation/foundation)
- [Swift CSV - A community-driven CSV parser for Swift](https://github.com/imasudan/SwiftCSV)
- [How to Read and Write CSV files in Swift](https://www.iosapptemplates.com/blog/swift-programming/csv-swift)