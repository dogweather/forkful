---
title:                "Swift recipe: Working with csv"
simple_title:         "Working with csv"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/working-with-csv.md"
---

{{< edit_this_page >}}

## Why Engage in Working with CSV?

CSV (Comma Separated Values) is a popular file format used for storing and exchanging data between different systems. It is widely used in data analysis, database management, and other computer applications. In the world of Swift programming, being able to work with CSV files can greatly increase your efficiency and productivity. In this blog post, we will explore how to work with CSV files using Swift, and why it is an important skill for any programmer to have.

## How To Work with CSV in Swift
To begin working with CSV files in Swift, we first need to import the `CSV` module from the `SwiftCSV` package. This can be done in the following way:

```Swift
import CSV
```

Next, we need to create a `CSV` object, passing in the path of the CSV file we want to work with. For example:

```Swift
let csvPath = "/Users/username/Documents/sample.csv"
let csv = try! CSV(url: csvPath, encoding: .utf8, delimiter: ",", loadColumns: false)
```

Once we have the `CSV` object, we can start accessing the data within the CSV file. The `CSV` object has a `namedRows` property which contains all the data in rows, with the first row being the header. Let's take a look at a simple example where we print out the data from the CSV file:

```Swift
for row in csv.namedRows {
    print(row)
}
```

This will output each row of data in the CSV file. We can also access a specific column by using the `namedColumns` property of the `CSV` object. For example, if we want to retrieve the data from the "Name" column, we can use the following code:

```Swift
let names = csv.namedColumns["Name"]
print(names) // Prints out an array of all the names in the "Name" column
```

By using these simple techniques, we can easily read and manipulate data from CSV files in our Swift programs.

## Deep Dive into Working with CSV
There are a few important things to keep in mind when working with CSV files in Swift. First, the CSV file must be properly formatted, with each column separated by a delimiter (usually a comma) and each row ending with a line break. It is also important to handle errors when reading from or writing to a CSV file. The `CSV` object allows us to catch errors and handle them appropriately.

Another important aspect to consider is the encoding of the CSV file. By default, the `CSV` object uses UTF-8 encoding, but this can be changed by passing in a different encoding option when creating the object.

It is also worth noting that the `CSV` object allows us to write data to CSV files as well. This can be done using the `write` method, which takes in an array of values as a parameter and writes them as a new row in the CSV file.

## See Also
- [SwiftCSV GitHub repository](https://github.com/yaslab/CSV.swift)
- [Working with CSV files in Swift tutorial](https://www.raywenderlich.com/542-yaml-tutorial-for-swift-working-with-csv-files)
- [Official Swift documentation](https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID384)