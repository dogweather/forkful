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

## Why
CSV (Comma Separated Values) is a widely used format for storing and exchanging data, making it essential for any programming language to have robust support for it. Elixir, with its functional and scalable nature, is a perfect choice for handling CSV data.

## How To
To start working with CSV in Elixir, you need to first install the CSV library. In your project's `mix.exs` file, add `{:csv, "~> 2.4"}` to the `deps` function. Then run `mix deps.get` to install the CSV library.

Next, you need to import the CSV module in your Elixir file:
```Elixir
import CSV
```

Now let's see some examples of working with CSV data. First, we will read a CSV file and store its data into a variable:
```Elixir
data = CSV.parse_file("~/my_data.csv")
```
This will give us a list of lists, where each inner list represents a row in the CSV file. 

To write data to a CSV file, we simply need to pass in a list of lists to the `write` function:
```Elixir
CSV.write("~/output.csv", [[1, 2], [3, 4], [5, 6]])
```
This will create a CSV file with the provided data.

You can also pass options to the `parse_file` and `write` functions to customize the CSV parsing and writing behavior. For example, you can specify the delimiter, quote character, and headers among other options.

## Deep Dive
The CSV library in Elixir comes with a variety of helper functions to make working with CSV data easier. Some of the most commonly used functions are `encode`, `decode`, `parse_stream`, and `write_stream`.

Additionally, the library also supports working with RFC 4180 format, which is widely used for CSV files. It also has support for working with CSV data encoded in different character sets.

## See Also
- [CSV library documentation](https://hexdocs.pm/csv)
- [Elixir official website](https://elixir-lang.org/)