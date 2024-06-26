---
date: 2024-02-03 19:03:10.829234-07:00
description: "How to: Elixir, with its powerful pattern matching and support for pipelining,\
  \ can handle CSV files efficiently, even without third-party libraries.\u2026"
lastmod: '2024-03-13T22:44:59.802302-06:00'
model: gpt-4-0125-preview
summary: Elixir, with its powerful pattern matching and support for pipelining, can
  handle CSV files efficiently, even without third-party libraries.
title: Working with CSV
weight: 37
---

## How to:
Elixir, with its powerful pattern matching and support for pipelining, can handle CSV files efficiently, even without third-party libraries. However, for more advanced needs, the `nimble_csv` library is a fast and straightforward choice.

### Reading a CSV File Without External Libraries
You can read and parse a CSV file by using Elixir's built-in functions:

```elixir
defmodule CSVReader do
  def read_file(file_path) do
    File.stream!(file_path)
    |> Stream.map(&String.trim_trailing/1)
    |> Stream.map(&String.split(&1, ","))
    |> Enum.to_list()
  end
end

# Sample usage
CSVReader.read_file("data.csv")
# Output: [["Header1", "Header2"], ["Row1Value1", "Row1Value2"], ["Row2Value1", "Row2Value2"]]
```

### Writing to a CSV File
Similarly, to write data to a CSV file:

```elixir
defmodule CSVWriter do
  def write_to_file(file_path, data) do
    File.open(file_path, [:write], fn file ->
      Enum.each(data, fn row ->
        IO.write(file, Enum.join(row, ",") <> "\n")
      end)
    end)
  end
end

# Sample usage
data = [["Header1", "Header2"], ["Value1", "Value2"], ["Value3", "Value4"]]
CSVWriter.write_to_file("output.csv", data)
# Creates output.csv with the data formatted as CSV
```

### Using `nimble_csv`
For more complex CSV handling, `nimble_csv` provides a powerful and flexible way to work with CSV data. First, add `nimble_csv` to your dependencies in `mix.exs` and run `mix deps.get`:

```elixir
defp deps do
  [
    {:nimble_csv, "~> 1.2"}
  ]
end
```

Parsing CSV data with `nimble_csv`:

```elixir
defmodule MyCSVParser do
  NimbleCSV.define(MyParser, separator: ",", escape: "\\")

  def parse(file_path) do
    file_path
    |> File.stream!()
    |> MyParser.parse_stream()
    |> Enum.to_list()
  end
end

# Sample usage
MyCSVParser.parse("data.csv")
# Output with nimble_csv can be customized based on the definition, but it generally looks like a list of lists or tuples depending on how you set up your parser.
```

Writing CSV data using `nimble_csv` requires manually transforming your data into a proper format and then writing it to a file, much like the plain Elixir example but leveraging `nimble_csv` for generating correctly formatted CSV rows.

By choosing the appropriate approach for your task's complexity, you can handle CSV files in Elixir with great flexibility and power.
