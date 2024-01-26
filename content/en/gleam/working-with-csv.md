---
title:                "Working with CSV"
html_title:           "C recipe: Working with CSV"
simple_title:         "Working with CSV"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?

Working with CSV (Comma-Separated Values) files means handling data in a simple, text-based format that stores tabular data. Programmers use it because it's widely supported, easy to read, and straightforward to parse or generate.

## How to:

Currently, Gleam does not have a dedicated standard library for CSV manipulation, but you can implement basic parsing with the built-in functions. Here's a simple example:

```gleam
import gleam/string

fn parse_csv_line(line: String) -> List(String) {
  string.split(line, ",")
}

pub fn main() {
  let csv_content = "name,age,city\nAlice,30,New York\nBob,22,Los Angeles"
  let lines = string.split(csv_content, "\n")
  case lines {
    [] -> []
    [.., _header | rows] -> rows
      |> list.map(parse_csv_line)
      |> io.debug
  }
}

// Sample output inside `main` function:
// [
//   ["Alice", "30", "New York"],
//   ["Bob", "22", "Los Angeles"],
// ]
```
Remember to handle edge cases like commas in values, newlines, and text qualifiers in a full implementation.

## Deep Dive

CSV is an old format, dating back to early computing, which contributes to its wide adoption. Alternatives like JSON or XML provide more structure but can be more complex to parse. How you handle CSV data in Gleam may involve using external libraries if available, or creating a custom parser. Serializing to CSV might require appending commas and newlines carefully, escaping necessary characters.

## See Also

- To understand different file formats, check out [JSON](https://www.json.org/json-en.html) and [XML](https://www.w3.org/XML/) specifications.
- For complex CSV handling, consider contributing to or using a CSV library in the [Gleam ecosystem](https://hex.pm/) when available.
