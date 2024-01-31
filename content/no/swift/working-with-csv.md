---
title:                "Arbeid med CSV"
date:                  2024-01-19
simple_title:         "Arbeid med CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
CSV står for "comma-separated values". Det brukes for å lagre og dele tabelldata fordi det er enkelt og universelt kompatibelt.

## How to:
Hvordan lese en CSV-fil i Swift:

```Swift
import Foundation

// Anta at du har en streng som representerer en CSV-fils innhold
let csvData = """
name,age,city
Alice,30,New York
Bob,25,Los Angeles
"""

// Splitt hver linje og parse innholdet
let rows = csvData.components(separatedBy: "\n")
var header: [String]?
var users = [[String: String]]()

for (index, row) in rows.enumerated() {
    let columns = row.components(separatedBy: ",")
    if index == 0 {
        header = columns
    } else {
        var userData = [String: String]()
        for (hIndex, column) in columns.enumerated() {
            if let title = header?[hIndex] {
                userData[title] = column
            }
        }
        users.append(userData)
    }
}

print(users)
```
Sample output:
```
[["name": "Alice", "age": "30", "city": "New York"], ["name": "Bob", "age": "25", "city": "Los Angeles"]]
```

## Deep Dive
CSV-formatet har vært i bruk siden 1970-tallet og er fremdeles populært på grunn av dets enkelhet. Alternativer til CSV inkluderer JSON og XML, men disse kan være overkill for enkle, tabulære data. Implementasjonen i Swift er rett fram, men vær obs på potensielle komplikasjoner som felt med komma, nye linjer eller tilpassede tegnsett.

## See Also
- Swift Programmeringsspråk Dokumentasjon: [swift.org/documentation](https://swift.org/documentation/)
- CSV Parsing bibliotek for Swift – SwiftCSV: [github.com/swiftcsv/SwiftCSV](https://github.com/swiftcsv/SwiftCSV)
