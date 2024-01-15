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

## Why
CSV (Comma Separated Values) is a common and user-friendly format for storing and exchanging tabular data. It's widely used in data analysis and manipulation, making it a valuable skill for Swift programmers to learn.

## How To
To get started working with CSV in Swift, you'll need to import the `CSV` library, which is available as a package in the Swift Package Manager. Here's a basic example of how to load and read data from a CSV file:

```Swift
import CSV

// Load the CSV file
let csv = try! CSV(url: URL(fileURLWithPath: "data.csv"))

// Loop through each row and access the columns
for row in csv {
  let name = row["Name"]
  let age = row["Age"]
  print("\(name) is \(age) years old")
}
```

The output of this code block will print out the names and ages of each person listed in the CSV file. You can also use the `rowsAsArrays` property to access the data in a more structured format:

```Swift
import CSV

// Load the CSV file
let csv = try! CSV(url: URL(fileURLWithPath: "data.csv"))

// Get array of arrays containing the data
let data = csv.rowsAsArrays

// Loop through each row and access the data by index
for row in data {
  let name = row[0]
  let age = row[1]
  print("\(name) is \(age) years old")
}
```

You can also write data to a CSV file using the `writeRow` method:

```Swift
import CSV

// Create a new CSV file to write to
let csv = try! CSV(url: URL(fileURLWithPath: "new_data.csv"))

// Write row of data to file
try! csv.write(row: ["John", "25", "Male"])

// Write another row
try! csv.write(row: ["Jane", "30", "Female"])
```

## Deep Dive
The `CSV` library in Swift also allows for more advanced use cases, such as customizing the delimiter character or using a header row. You can also access and modify the raw rows of data using the `rawRows` property.

Additionally, you can use `CSVReader` to read from a CSV file without loading the entire file into memory. This can be useful for processing large CSV files with minimal performance impact.

## See Also
For more information and examples, check out the official documentation for `CSV` library: [https://github.com/yaslab/CSV.swift](https://github.com/yaslab/CSV.swift)