---
title:                "Att arbeta med csv"
html_title:           "Swift: Att arbeta med csv"
simple_title:         "Att arbeta med csv"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/working-with-csv.md"
---

{{< edit_this_page >}}

# Vad & Varför?
 CSV står för Comma Separated Values och är ett vanligt format för att lagra och utbyta tabell-data. Programerare kan behöva arbeta med CSV-filer för att importera eller exportera data till eller från en databas eller för att hantera data som är läsbar för mänskliga. 

# Hur man:

```Swift
import Foundation
import SwiftCSV

// Skapa en CSV-fil
let csvFilePath = "example.csv"
let csvFile = try! CSV(string: "Name,Age,Country\nJohn,25,Sweden\nLisa,30,USA")

// Exportera till en CSV-fil
try! csvFile.write(to: csvFilePath)

// Läsa från en CSV-fil
let csvFile = try! CSV(name: csvFilePath)
let rows = csvFile.rows
let firstRow = rows[0]
let name = firstRow["Name"]
let age = firstRow["Age"]
let country = firstRow["Country"]

// Modifera en CSV-fil
let csvFile = try! CSV(name: csvFilePath)
var rows = csvFile.rows

// Lägg till en ny rad
rows.append(["Peter", "45", "Denmark"])

// Uppdatera en befintlig rad
rows[0]["Age"] = "26"

// Spara ändringar till en CSV-fil
try! csvFile.write(to: csvFilePath)
```

# Djupdykning:
CSV-filer har funnits sedan 1970-talet och har varit en viktig del av datautbyte mellan system och applikationer. Idag finns det olika format och varianter av CSV och det är viktigt att ha en korrekt parser för att kunna hantera filerna korrekt. Alternativ till att jobba med CSV inkluderar XML, JSON och databasformat som SQL.

# Se även:
- SwiftCSV GitHub Repository: https://github.com/naithar/SwiftCSV
- Dokumentation för Foundation's CSV framework: https://developer.apple.com/documentation/foundation/csv