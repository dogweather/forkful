---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:22.102900-07:00
description: 'Hoe: Laten we een CSV-bestand lezen en de inhoud ervan parseren in Swift.
  Eerst gaan we ervan uit dat we een `data.csv` bestand hebben met deze inhoud.'
lastmod: '2024-03-13T22:44:51.180351-06:00'
model: gpt-4-0125-preview
summary: Laten we een CSV-bestand lezen en de inhoud ervan parseren in Swift.
title: Werken met CSV
weight: 37
---

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
