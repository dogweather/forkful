---
title:                "Working with csv"
html_title:           "Swift recipe: Working with csv"
simple_title:         "Working with csv"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
CSV (Comma Separated Values) is a type of tabular data format commonly used for storing and exchanging structured data. It consists of rows and columns, where each row represents an entry and each column represents a data field. Programmers use CSV because it is a lightweight and easy-to-read format that can be easily manipulated and analyzed, making it an efficient and popular choice for handling large datasets.

## How to:
To work with CSV in Swift, you will need to import the `Foundation` framework. Then, you can use the `Data(contentsOf:options:)` initializer to read the contents of a CSV file and convert it into a `String` object. From there, you can use the `components(separatedBy:)` method to break the string into separate rows and columns.

```Swift
import Foundation

let csvString = try String(contentsOf: URL(fileURLWithPath: "data.csv"), encoding: .utf8)
let rows = csvString.components(separatedBy: "\n")
for row in rows {
  let columns = row.components(separatedBy: ",")
  for column in columns {
    print(column)
  }
}
```

The code above reads the contents of a CSV file called "data.csv" and splits it into rows and columns, printing each value on a separate line.

## Deep Dive:
CSV was first introduced in the 1970s as a way to store and exchange data between different computer systems. It gained popularity in the 1990s with the rise of the Internet and the need to transfer large amounts of data. Today, it remains one of the most widely used data formats, especially in industries such as finance, marketing, and sales.

There are other file formats that can be used for storing and manipulating tabular data, such as JSON and XML. However, CSV has the advantage of being human-readable and easily editable, making it a popular choice among programmers.

In terms of implementation, CSV files can vary in terms of their delimiter (comma, semicolon, tab, etc.) and data enclosures (quotes, double quotes, etc.). Therefore, it's important to carefully examine the structure of the CSV file before attempting to parse it.

## See Also:
- [Hacking with Swift: Reading CSV data](https://www.hackingwithswift.com/example-code/strings/how-to-load-a-string-from-a-website-url)