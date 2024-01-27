---
title:                "Arbeta med csv"
date:                  2024-01-19
html_title:           "Arduino: Arbeta med csv"
simple_title:         "Arbeta med csv"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/working-with-csv.md"
---

{{< edit_this_page >}}

## Vad & Varför?
CSV, "Comma-Separated Values", är textfiler där data separeras med kommatecken. Programmerare använder CSV för enkelheten att lagra och utbyta data mellan olika system.

## How to:
För att jobba med CSV-filer i Swift kan man använda `String`-metoder för att dela upp innehållet. Nedan ser du hur man kan läsa och skriva CSV-data.

```Swift
import Foundation

// Exempel på CSV-sträng
let csvString = """
Name,Age,Job
Alice,35,Developer
Bob,30,Designer
"""

// Läsa CSV
func readCSV(contents: String) -> [[String]] {
    let rows = contents.components(separatedBy: "\n").filter { !$0.isEmpty }
    return rows.map { $0.components(separatedBy: ",") }
}

let data = readCSV(contents: csvString)
print(data)

// Skriva till CSV
func writeCSV(data: [[String]]) -> String {
    var contents = ""
    for row in data {
        contents += row.joined(separator: ",") + "\n"
    }
    return contents
}

let newCSVString = writeCSV(data: data)
print(newCSVString)
```

Sample output:
```
[["Name", "Age", "Job"], ["Alice", "35", "Developer"], ["Bob", "30", "Designer"]]
Name,Age,Job
Alice,35,Developer
Bob,30,Designer
```

## Deep Dive
CSV-formatet har historiskt sett varit ett enkelt sätt att överföra tabellformad data sedan 1970-talet. Alternativ till CSV inkluderar JSON och XML som också hanterar hierarkisk data. Swifts `Codable`-protokoll kan användas för att mappa CSV-innehåll till anpassade datamodeller vilket ger starkare typsäkerhet och bättre hantering av komplex data.

## See Also
- [Apple Developer Documentation: Codable](https://developer.apple.com/documentation/swift/codable)
- [SwiftCSV – ett annat CSV-bibliotek](https://github.com/swiftcsv/SwiftCSV)
