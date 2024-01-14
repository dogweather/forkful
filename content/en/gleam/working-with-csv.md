---
title:                "Gleam recipe: Working with csv"
simple_title:         "Working with csv"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/working-with-csv.md"
---

{{< edit_this_page >}}

## Why
CSV (Comma Separated Values) is a common file format used for storing data in tabular form, making it easy to read and manipulate. Working with CSV files can be beneficial for data management, analysis, and integration with other programs. In this blog post, we will be looking at how to work with CSV files using the Gleam programming language.

## How To
To start working with CSV files in Gleam, we first need to import the standard library `gleam/csv` module. This module provides functions for reading, parsing, and writing CSV files. Let's take a look at an example of how to read a CSV file and print its contents:

```Gleam
import gleam/csv

// Read a CSV file
let csv = csv.read("data.csv")

// Loop through each row
for row in csv {
  // Print each column in the row
  for col in row {
    std.io.println(col)
  }
}
```

The output of this code block would be each cell in the CSV file printed on a new line. We can also specify the delimiter used in the CSV file, as well as skip the header row by passing in optional arguments to the `csv.read` function.

Now, let's see how we can write data to a CSV file using the `csv.write` function:

```Gleam
import gleam/csv

// Create some data to write to the CSV file
let headers = ["Name", "Age", "Occupation"]
let rows = [
  ["John", "30", "Software Engineer"],
  ["Jane", "28", "Data Analyst"],
  ["Mark", "35", "Project Manager"]
]

// Write the data to a CSV file
csv.write("output.csv", [headers | rows])
```

This code will create a CSV file with the specified headers and rows of data.

## Deep Dive
In addition to basic reading and writing of CSV files, the `gleam/csv` module also provides functions for more advanced operations such as appending and deleting rows, sorting data, and converting CSV files to other formats such as JSON. It also supports handling special cases like empty cells and value escaping. For more information and examples, be sure to check out the official documentation for this module.

## See Also
- Official Gleam documentation: https://gleam.run/documentation/
- `gleam/csv` module documentation: https://gleam.run/modules/csv.html
- Tutorial on working with data in Gleam: https://medium.com/@drazisil/csv-manipulation-in-gleam-84503a1fdf3f