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
Comma-Separated Values (CSV) is a file format that stores tabular data. Developers often work with CSV files to handle and analyze large data sets due to its simplicity and wide support.

## How to:

Here is how you can read and write CSV files using Elixir:

First, add the following to your `mix.exs` dependencies:

```Elixir
defp deps do
  [
    {:csv, "~> 2.0"}
  ]
end
```

Then, install dependencies with `mix deps.get`.

To read a CSV file:

```Elixir
{:ok, data} = File.stream!("example.csv") |> CSV.decode
```
For writing a CSV file:

```Elixir
data = [ ["name", "age"], ["Alice", "25"], ["Bob", "30"] ]

{:ok, file} = File.open("new.csv", [:write])

CSV.encode(data) |> Enum.into(file)
```
## Deep Dive

CSV was first formalized in 2005 by the Internet Engineering Task Force (IETF). It's a de facto standard more than it is an official one. In Elixir, dealing with CSV files is straightforward using the CSV library, but there are caveats. Due to its implementation, it doesn't support complex CSV features like multi-line fields.

There are alternatives to CSV such as JSON, XML, etc., but the choice of format depends heavily on the use case. CSV is popular for its simplicity, widespread use, and compatibility with spreadsheet programs like MS Excel.

## See Also

- Elixir CSV library documentation: https://hexdocs.pm/csv/readme.html
- Internet Engineering Task Force (IETF): https://www.ietf.org/
- JSON vs XML vs CSV: https://www.geeksforgeeks.org/difference-between-xml-and-csv/