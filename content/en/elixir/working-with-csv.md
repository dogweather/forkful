---
title:                "Working with csv"
html_title:           "C recipe: Working with csv"
simple_title:         "Working with csv"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?

CSV (Comma-Separated Values) is a plain text format for tabular data. Programmers use CSV to exchange large datasets easily between different programs, services, or databases where complexity isn't a requirement.

## How to:

Elixir doesn't include CSV parsing in its standard library, but you can use the `CSV` hex package. Here's a quick example to get you started:

```elixir
# First, add `{:csv, "~> 2.4"}` to your mix.exs and run `mix deps.get`
# Then, use the CSV module like so:

CSV.decode("name,age\nJohn Doe,27\nJane Smith,32", headers: true)
|> Enum.map(fn(row) -> 
  "Hello, #{row["name"]} who is #{row["age"]} years old!"
end)
```

Sample output:

```
["Hello, John Doe who is 27 years old!", "Hello, Jane Smith who is 32 years old!"]
```

## Deep Dive

CSV isn't new; it's been around since the early 1970s, making it one of the most enduring file formats. Its simplicity is its greatest strength and weakness. Alternatives include JSON, XML, or binary formats like Protocol Buffers, each with their trade-offs in complexity, size, and readability. Regarding Elixir, when you decode CSV data using the `CSV` package, underneath the hood, it seamlessly handles common issues like data type conversion, escapes, and character encoding.

## See Also

- The `CSV` hex package documentation: <https://hexdocs.pm/csv>
- An introduction to Elixir's Stream module for large CSVs: <https://elixir-lang.org/getting-started/enumerables-and-streams.html>
- A comparison of file formats (CSV, JSON, XML, etc.): <https://www.ibm.com/docs/en/iis/11.5?topic=formats-comparing-file-reactivity>