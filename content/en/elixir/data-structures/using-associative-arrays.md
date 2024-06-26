---
date: 2024-01-30 18:57:19.478681-07:00
description: 'How to: Creating a Map is straightforward. You use the `%{}` syntax,
  like so.'
lastmod: '2024-03-13T22:44:59.776360-06:00'
model: gpt-4-0125-preview
summary: Creating a Map is straightforward.
title: Using associative arrays
weight: 15
---

## How to:
Creating a Map is straightforward. You use the `%{}` syntax, like so:

```elixir
my_map = %{"name" => "Alex", "age" => 32}
IO.inspect(my_map)
```

Accessing values is done by using the keys:

```elixir
IO.puts my_map["name"]
```
Output: `Alex`

To add or update values, you can use the `Map.put/3` function:

```elixir
updated_map = Map.put(my_map, "location", "NY")
IO.inspect(updated_map)
```
Output: `%{"age" => 32, "location" => "NY", "name" => "Alex"}`

Removing keys is just as simple with `Map.delete/2`:

```elixir
trimmed_map = Map.delete(updated_map, "age")
IO.inspect(trimmed_map)
```
Output: `%{"location" => "NY", "name" => "Alex"}`

## Deep Dive
Maps in Elixir are an evolution of the older key-value storage types, like Hashes in Ruby or Dictionaries in Python. They allow for more efficient lookups and insertions, making them a go-to for modern Elixir programming. It's worth noting that before Maps, Elixir used HashDict and Dict modules, which are deprecated now.

However, for scenarios requiring ordered data, you might look at keyword lists in Elixir. These are lists of tuples, efficient for smaller collections but not as performance-friendly for large datasets as Maps.

Keep in mind that Maps store their keys in a "flat" structure, making direct access to nested values a bit tricky. For deep nesting, you might consider structured access via the `get_in`, `put_in`, `update_in`, and `get_and_update_in` functions, which allow for a more dynamic approach to nested data manipulation.

In sum, while Maps are your go-to for associative array needs in Elixir, the language offers a rich variety of data structures for every scenario, encouraging you to pick the right tool for the job.
