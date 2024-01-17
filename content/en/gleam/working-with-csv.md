---
title:                "Working with csv"
html_title:           "Gleam recipe: Working with csv"
simple_title:         "Working with csv"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/working-with-csv.md"
---

{{< edit_this_page >}}

GLEAM

## What & Why?
CSV (Comma-Separated Values) is a popular file format used to store tabular data in a simple and lightweight way. Programmers often work with CSV files because they are human-readable and can be easily imported into most applications or databases.

## How to:
To work with CSV files in Gleam, we use the `gleam/csv` module. First, we import it into our project:

```Gleam
import gleam/csv
```

Then, we can use the `csv.read` function to read data from a CSV file:

```Gleam
// Read CSV data from a file
let result = csv.read("my_data.csv")

// Check if the data was successfully read
case result {
  Ok(data) -> data // Do something with the data
  Error(error) -> error // Handle any errors that may occur
}
```

We can also pass a custom delimiter character to the `read` function if our CSV file is not comma-separated:

```Gleam
let result = csv.read("my_data.csv", ";") // Reads a semicolon-separated CSV
```

Once we have the data, we can then manipulate it as needed using the `gleam/csv` functions, such as `csv.filter` or `csv.sort`.

To write data to a CSV file, we use the `csv.write` function:

```Gleam
// Create a list of rows to write to our file
let rows = [
  ["Name", "Age", "City"],
  ["John", 30, "New York"],
  ["Jane", 25, "London"],
  ["Sarah", 35, "Tokyo"]
]

// Write the rows to a CSV file
csv.write("my_data.csv", rows)
```

This will create a CSV file with the data from our rows list, with each row separated by a newline and each column separated by a comma.

## Deep Dive
CSV was first introduced in the 1970s and has since become a popular file format due to its simplicity and wide support. However, it has its limitations, such as not being able to store complex data structures. As an alternative, programmers may choose to use other file formats, such as JSON or XML, depending on their needs.

The `gleam/csv` module uses the `abnf_parser` library to parse the CSV data, which adheres to the RFC 4180 specification for CSV files. This ensures consistent and standardized parsing of CSV documents.

## See Also
- [Documentation for the `gleam/csv` module](https://gleam.run/modules/csv.html)
- [RFC 4180 specification for CSV files](https://tools.ietf.org/html/rfc4180)
- [ABNF Parser library for Gleam](https://github.com/gleam-lang/abnf_parser)