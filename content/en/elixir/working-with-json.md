---
title:                "Working with json"
html_title:           "Arduino recipe: Working with json"
simple_title:         "Working with json"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON (JavaScript Object Notation) is a lightweight data-interchange format that's easy for humans to read and write and easy for machines to parse and generate. Programmers work with JSON to exchange data between servers and web applications, store configuration, or serialize data for network communication.

## How to:

To handle JSON in Elixir, we use libraries like `Jason` or `Poison`. Here's a quick how-to with `Jason`:

```elixir
# Add Jason to your mix.exs as a dependency
{:jason, "~> 1.3"}

# in a .ex file, to encode Elixir to JSON
json_string = Jason.encode!(%{foo: "bar"})

# Now decoding JSON to Elixir
elixir_map = Jason.decode!(json_string)
```

Output:

```elixir
json_string #=> "{\"foo\":\"bar\"}"
elixir_map  #=> %{"foo" => "bar"}
```

Encode with `opts` for pretty printing:

```elixir
Jason.encode!(%{foo: "bar"}, pretty: true)
```

Output:

```json
{
  "foo": "bar"
}
```

## Deep Dive

JSON was proposed by Douglas Crockford in the early 2000s. It quickly gained adoption due to its simplicity over XML.

Alternatives? Sure—XML, YAML, or Protocol Buffers, yet JSON reigns due to simplicity and JavaScript native support.

Under the hood, JSON libraries convert Elixir data types into JSON strings and vice versa. Elixir's pattern matching and robust standard library make the encoding and decoding process smooth.

## See Also

- Jason GitHub: https://github.com/michalmuskala/jason
- Poison GitHub: https://github.com/devinus/poison
- Elixir School JSON lessons: https://elixirschool.com/en/lessons/specifics/jason/