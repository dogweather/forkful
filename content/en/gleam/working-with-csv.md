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

## What & Why?

Working with CSV (Comma Separated Values) files involves reading from and writing data to CSV format. Programmers work with CSV as it's a widely accepted, simple, and human-friendly data format.

## How to:

In Gleam, this task can be executed by creating custom functions, but for now, let's focus on reading CSV files.

```Gleam
import gleam/row.{Row, from_list, to_list}
import gleam/string

fn read_csv(content: String) -> List(Row(String)) {
  content
  |> string.split_on("\n")
  |> list.map(split_fields)
  |> list.map(from_list)
}

fn split_fields(line: String) -> List(String) {
  string.split_on(line, ",")
}
```

The `read_csv` function breaks down a CSV content into rows, while `split_fields` splits a single row into individual fields. 

For a CSV content `"Alice,28\nBob,30"`, the output would be:
```Gleam
[
 Row(from_list(["Alice", "28"])),
 Row(from_list(["Bob", "30"]))
]
```

Writing to CSV can be done with a similar approach. 

## Deep Dive

CSV, first introduced in the 70s, is popular due to its simplicity and broad application. However, it lacks expressiveness, doesn’t handle complex types, and could be inefficient for large datasets. Thus, formats like JSON, XML or even database-specific file formats are often chosen for more complex scenarios. 

The provided Gleam implementation maps directly onto lists. It relies on the simplicity of Gleam's standard library and adheres to Gleam's design philosophy of simplicity and immutability. However, it considers all fields as strings and doesn't support changing types.

## See Also

1. Gleam Documentation: [https://gleam.run/docs/](https://gleam.run/docs/)
2. The Gleam CSV Issue: [https://github.com/gleam-lang/](https://github.com/gleam-lang/)
3. Gleam Community: [https://gleam.run/community/](https://gleam.run/community/)