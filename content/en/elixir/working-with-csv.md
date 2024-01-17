---
title:                "Working with csv"
html_title:           "Elixir recipe: Working with csv"
simple_title:         "Working with csv"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
Working with CSV (Comma Separated Values) in Elixir is a way to easily manipulate tabular data stored in text files or databases. Programmers often use CSV to transfer data between systems or to analyze large datasets, as it provides a simple and flexible format for organizing data.

## How to:
To read a CSV file in Elixir, we can use the built-in CSV library and its ```parse_file/2``` function. This function takes in the file path of the CSV file and returns a list of lists where each inner list represents a row in the CSV file. Here's an example:

```
Elixir
rows = CSV.parse_file("my_data.csv")
# [["Name", "Age", "Country"],
#  ["John", "25", "USA"],
#  ["Jane", "30", "Canada"]]
```

To write a list of data to a CSV file, we can use the ```write/2``` function from the CSV library. This function takes in a list of lists and a file path and will write the data to the specified file. Here's an example:

```
Elixir
rows = [
  ["Name", "Age", "Country"],
  ["John", "25", "USA"],
  ["Jane", "30", "Canada"]
]

CSV.write(rows, "my_data.csv")
# The CSV file "my_data.csv" is created with the data from the list of lists.
```

## Deep Dive:
CSV was first introduced in the early 1970s and has become a widely used data format due to its simplicity and compatibility with various software systems. While CSV is a popular choice for storing data, other alternatives such as JSON or XML may be preferred for certain use cases.

In Elixir, the CSV library is included in the standard library, making it easily accessible for developers. This library also provides functions for customizing the delimiter and handling headers in the CSV file.

## See Also:
- [Elixir CSV documentation](https://hexdocs.pm/csv/CSV.html)
- [CSV file format](https://tools.ietf.org/html/rfc4180)