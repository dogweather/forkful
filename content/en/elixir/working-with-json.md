---
title:                "Working with json"
html_title:           "Elixir recipe: Working with json"
simple_title:         "Working with json"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/working-with-json.md"
---

{{< edit_this_page >}}

## Why

If you're working in the world of web development, chances are you've come across JSON (JavaScript Object Notation) in your work. JSON has become the standard way of exchanging data between web browsers and servers, making it a crucial skill to have. Elixir makes working with JSON a breeze, making it a language of choice for web developers looking to improve their productivity.

## How To

```elixir
# Importing the Jason library
iex> {:ok, json} = HTTPoison.get("https://jsonplaceholder.typicode.com/todos/1")
iex> response = JSON.decode!(json.body)
```

This code block demonstrates how easy it is to import the Jason library and use it to retrieve and decode JSON data from a URL. The response variable will now contain a decoded representation of the JSON data, ready for further manipulation or use. You can also use the `Jason.encode()` function to convert Elixir data structures into JSON.

## Deep Dive

Elixir's Jason library uses Erlang's jiffy library under the hood, which is known for its fast and efficient JSON parsing and encoding capabilities. Jason supports most of the commonly used options for customizing the encoding and decoding process, such as specifying keys to include or exclude, customizing number formats, and more. Furthermore, Elixir's built-in pattern matching allows for easy extraction of specific data from the JSON response.

## See Also

- [Jason Documentation](https://hexdocs.pm/jason/)
- [Elixir's Built-In Pattern Matching Guide](https://elixir-lang.org/getting-started/pattern-matching.html)
- [Elixir School - Working with JSON](https://elixirschool.com/en/lessons/specifics/json/)