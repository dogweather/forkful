---
title:                "Elixir recipe: Working with csv"
simple_title:         "Working with csv"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/working-with-csv.md"
---

{{< edit_this_page >}}

## Why

CSV (Comma-Separated Values) files are a common way to store tabular data in a simple, human-readable format. Working with CSV files can be helpful for tasks such as data analysis, data migration, and data storage. In this blog post, we will explore how to use Elixir to work with CSV files and perform various operations on them.

## How To

The Elixir standard library provides a module called `CSV` which allows us to easily read and write CSV files. Let’s take a look at some examples:

### Reading a CSV File

To read a CSV file, we can use the `CSV.decode/2` function. The first argument is the path to the CSV file, and the second argument is a keyword list of options. Here’s an example:

```elixir
{:ok, data} = File.read("users.csv") #read the CSV file
users = CSV.decode(data, headers: true) #convert the data into a list of maps
```

In the above example, we first use `File.read/1` to read the contents of the file and then use `CSV.decode/2` to convert it into a list of maps. The `headers: true` option tells the function that the first row of the file contains the column headers.

### Writing to a CSV File

To write data to a CSV file, we can use the `CSV.encode/2` function. The first argument is the data we want to write, and the second argument is a keyword list of options. Here’s an example:

```elixir
users = [
  %{id: 1, name: "John", age: 25},
  %{id: 2, name: "Jane", age: 30},
  %{id: 3, name: "Bob", age: 40}
]

File.write("users.csv", CSV.encode(users, headers: [:id, :name, :age]))
```

In the above example, we have a list of maps representing user data, and we use `CSV.encode/2` to convert it into a CSV string and write it to a file. The `headers: [:id, :name, :age]` option specifies the order of columns in the CSV file.

### Customizing Delimiters

By default, `CSV` uses a comma (`,`) as a delimiter between values. However, we can customize this by passing the `:separator` option to `CSV.decode/2` or `CSV.encode/2`. Here’s an example:

```elixir
users = CSV.decode(data, headers: true, separator: ";") #reads a CSV file with semicolon separated values
```

## Deep Dive

The `CSV` module also provides functions for handling other aspects of CSV files, such as quoting characters and row/field separators. For a detailed overview of all available functions and options, check out the [CSV module documentation](https://hexdocs.pm/elixir/CSV.html).

## See Also

- [Elixir CSV module documentation](https://hexdocs.pm/elixir/CSV.html)
- [Elixir File module documentation](https://hexdocs.pm/elixir/File.html)
- [Elixir keyword lists documentation](https://hexdocs.pm/elixir/Keyword.html)

By now, you should have a good understanding of how to work with CSV files in Elixir. With its user-friendly API and customizable options, the `CSV` module makes it easy to manipulate and process CSV data. Happy coding!