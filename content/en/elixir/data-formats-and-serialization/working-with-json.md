---
date: 2024-02-03 19:03:07.967968-07:00
description: "Working with JSON involves parsing JSON-formatted strings into data\
  \ structures that Elixir can manipulate, and serializing Elixir data structures\
  \ back\u2026"
lastmod: '2024-03-13T22:44:59.801441-06:00'
model: gpt-4-0125-preview
summary: Working with JSON involves parsing JSON-formatted strings into data structures
  that Elixir can manipulate, and serializing Elixir data structures back into JSON
  strings.
title: Working with JSON
weight: 38
---

## What & Why?

Working with JSON involves parsing JSON-formatted strings into data structures that Elixir can manipulate, and serializing Elixir data structures back into JSON strings. This is essential for web development, APIs, and configuration files, as JSON is a lightweight, text-based, language-independent data exchange format widely used for its simplicity and human-readability.

## How to:

In Elixir, you can use the `Jason` library, a popular choice for JSON parsing and generation. First, add `Jason` to your project's dependencies in `mix.exs`:

```elixir
defp deps do
  [
    {:jason, "~> 1.3"}
  ]
end
```

Then, run `mix deps.get` to fetch the dependency.

### Parsing JSON:
To convert a JSON string into Elixir data structures:

```elixir
json_string = "{\"name\":\"John\", \"age\":30}"
{:ok, person} = Jason.decode(json_string)
IO.inspect(person)
# Output: %{"name" => "John", "age" => 30}
```

### Generating JSON:
To convert an Elixir map into a JSON string:

```elixir
person_map = %{"name" => "Jane", "age" => 25}
{:ok, json_string} = Jason.encode(person_map)
IO.puts(json_string)
# Output: {"age":25,"name":"Jane"}
```

### Working with Structs:
To encode an Elixir struct, you must implement the `Jason.Encoder` protocol for your struct. Here's an example:

```elixir
defmodule Person do
  @derive {Jason.Encoder, only: [:name, :age]}
  defstruct name: nil, age: nil
end

person_struct = %Person{name: "Mike", age: 28}
{:ok, json_string} = Jason.encode(person_struct)
IO.puts(json_string)
# Output: {"age":28,"name":"Mike"}
```

This simple approach will get you started on integrating JSON processing into your Elixir applications, facilitating data interchange in various programming environments.
