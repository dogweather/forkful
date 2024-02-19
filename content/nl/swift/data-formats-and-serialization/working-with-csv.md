---
aliases:
- /nl/swift/working-with-csv/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:22.102900-07:00
description: "CSV (Comma-Separated Values) bestanden zijn eenvoudige tekstbestanden\
  \ voor het opslaan van tabelgegevens. Programmeurs gebruiken ze omdat ze gemakkelijk\u2026"
lastmod: 2024-02-18 23:09:02.253848
model: gpt-4-0125-preview
summary: "CSV (Comma-Separated Values) bestanden zijn eenvoudige tekstbestanden voor\
  \ het opslaan van tabelgegevens. Programmeurs gebruiken ze omdat ze gemakkelijk\u2026"
title: Werken met CSV
---

{{< edit_this_page >}}

## Wat & Waarom?

CSV (Comma-Separated Values) bestanden zijn eenvoudige tekstbestanden voor het opslaan van tabelgegevens. Programmeurs gebruiken ze omdat ze gemakkelijk te lezen en te schrijven zijn, en breed ondersteund worden over systemen en talen heen.

## Hoe:

Laten we een CSV-bestand lezen en de inhoud ervan parseren in Swift.

Eerst gaan we ervan uit dat we een `data.csv` bestand hebben met deze inhoud:

```plaintext
name,age,city
Alice,30,New York
Bob,25,Los Angeles
```

Hier is een basis Swift-script om het te lezen en te parseren:

```swift
import Foundation

let csvContent = """
name,age,city
Alice,30,New York
Bob,25,Los Angeles
"""

var rows = csvContent.components(separatedBy: "\n")
let headers = rows.removeFirst().components(separatedBy: ",")

var data = [[String: String]]()

for row in rows {
    let columns = row.components(separatedBy: ",")
    var rowData = [String: String]()
    for (header, column) in zip(headers, columns) {
        rowData[header] = column
    }
    data.append(rowData)
}

print(data)
```

Voorbeelduitvoer:

```plaintext
[["name": "Alice", "age": "30", "city": "New York"], ["name": "Bob", "age": "25", "city": "Los Angeles"]]
```

## Diepgaand

CSV bestaat al sinds de vroege computertijd - gebruikt voor het verplaatsen van gegevens tussen programma's, databases en systemen. Alternatieven zoals JSON en XML bestaan, maar CSV blijft populair vanwege zijn eenvoudigheid. Qua efficiëntie hanteren Swift's `String` methoden CSV's goed voor kleine datasets, maar grootschalige gegevens zouden wellicht een gespecialiseerde bibliotheek zoals SwiftCSV of CodableCSV nodig hebben voor prestatie en gemak.

## Zie Ook

- Apple's Swift documentatie voor String manipulatie: [https://developer.apple.com/documentation/swift/string](https://developer.apple.com/documentation/swift/string)
- SwiftCSV, een gespecialiseerde CSV-bibliotheek voor Swift: [https://github.com/swiftcsv/SwiftCSV](https://github.com/swiftcsv/SwiftCSV)
- CodableCSV, een CSV-encoder/decoder voor Swift: [https://github.com/dehesa/CodableCSV](https://github.com/dehesa/CodableCSV)
