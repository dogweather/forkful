---
title:                "Swift: Arbeta med csv"
simple_title:         "Arbeta med csv"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/working-with-csv.md"
---

{{< edit_this_page >}}

## Varför

CSV-filer är en vanlig typ av filer som används för att lagra och utbyta tabellinformation. Dessa filer är lätta att läsa och kan enkelt öppnas och redigeras i en textredigerare. Genom att lära sig hur man hanterar CSV-filer i Swift, kan du enkelt automatisera uppgifter såsom dataimport och export, vilket sparar dig tid och arbete.

## Hur man gör

Att läsa och skriva CSV-filer i Swift kan enkelt göras med hjälp av inbyggda språkfunktioner. För att läsa en CSV-fil kan du använda en `InputStream` och `CSVReader` från Swift Package Manager-biblioteket `swift-csv`.

```Swift
import Foundation
import CSV

// Öppna filen för läsning
let stream = InputStream(fileAtPath: "väg/till/filen.csv")!
let csv = try! CSVReader(stream: stream)

// Iterera över varje rad i filen
while let row = csv.next() {
    // Gör något med varje rad
    print(row)
}
```

För att skriva till en CSV-fil kan du använda `CSVWriter` från samma bibliotek.

```Swift
import Foundation
import CSV

// Öppna filen för skrivning
let outputFile = OutputStream(toFileAtPath: "väg/till/nyFil.csv", append: false)!
let csv = try! CSVWriter(stream: outputFile)

// Skriv en rad till filen
try! csv.write(row: ["Kolumn 1", "Kolumn 2", "Kolumn 3"])
```

Som du kan se är det väldigt enkelt att hantera CSV-filer i Swift. Se till att utforska de olika funktionerna och anpassa dem efter dina behov.

## Djupdykning

När du arbetar med CSV-filer i Swift finns det vissa saker att tänka på. Till exempel finns det olika sätt att formatera data i en CSV-fil, såsom olika sorters avgränsare och formatting för datum och tal. Det är också viktigt att hantera fel och undvika att förlora data när man bearbetar CSV-filer.

Det finns många resurser tillgängliga online som kan hjälpa dig att förbättra dina kunskaper om att arbeta med CSV-filer i Swift. Ta gärna en titt på Swift Package Manager-biblioteket `swift-csv` och dess dokumentation för mer detaljerad information.

## Se även

- [Swift Package Manager - SwiftCSV](https://github.com/swiftcsv/SwiftCSV)
- [Hur man hanterar CSV-filer i Swift](https://medium.com/swlh/working-with-csv-files-in-swift-4c74c4bbad2a)
- [En komplett guide till CSV-filer](https://www.computerhope.com/issues/ch001356.htm)