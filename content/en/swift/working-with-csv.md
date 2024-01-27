---
title:                "Working with CSV"
date:                  2024-01-19
html_title:           "C recipe: Working with CSV"
simple_title:         "Working with CSV"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?

CSV (Comma-Separated Values) files are simple text files for storing tabular data. Programmers use them because they're easy to read and write, and widely supported across systems and languages.

## How to:

Let's read a CSV file and parse its content in Swift.

First, let's assume we have a `data.csv` file with this content:

```plaintext
name,age,city
Alice,30,New York
Bob,25,Los Angeles
```

Here's a basic Swift script to read and parse it:

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

Sample output:

```plaintext
[["name": "Alice", "age": "30", "city": "New York"], ["name": "Bob", "age": "25", "city": "Los Angeles"]]
```

## Deep Dive

CSV has been around since the early computer days—used for moving data between programs, databases, and systems. Alternatives like JSON and XML exist, but CSV remains popular for its simplicity. Efficiency-wise, Swift's `String` methods handle CSVs well for small datasets, but large-scale data might need a specialized library like SwiftCSV or CodableCSV for performance and convenience.

## See Also

- Apple’s Swift documentation for String manipulation: [https://developer.apple.com/documentation/swift/string](https://developer.apple.com/documentation/swift/string)
- SwiftCSV, a dedicated CSV library for Swift: [https://github.com/swiftcsv/SwiftCSV](https://github.com/swiftcsv/SwiftCSV)
- CodableCSV, a CSV encoder/decoder for Swift: [https://github.com/dehesa/CodableCSV](https://github.com/dehesa/CodableCSV)
