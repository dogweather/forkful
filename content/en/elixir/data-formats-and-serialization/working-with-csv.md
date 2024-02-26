---
date: 2024-02-03 19:03:10.829234-07:00
description: "Working with CSV (Comma-Separated Values) files involves reading from\
  \ and writing data to these files, a common necessity for tasks requiring data\u2026"
lastmod: '2024-02-25T18:49:56.265206-07:00'
model: gpt-4-0125-preview
summary: "Working with CSV (Comma-Separated Values) files involves reading from and\
  \ writing data to these files, a common necessity for tasks requiring data\u2026"
title: Working with CSV
---

{{< edit_this_page >}}

## What & Why?

Working with CSV (Comma-Separated Values) files involves reading from and writing data to these files, a common necessity for tasks requiring data import/export or simple storage solutions. Programmers leverage this functionality for data interchange between systems, quick data editing, or for situations where a lightweight and easily manipulable data format is advantageous.

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
