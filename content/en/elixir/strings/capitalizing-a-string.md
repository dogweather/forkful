---
date: 2024-02-03 19:02:30.450485-07:00
description: "How to: Elixir provides a straightforward way to capitalize strings\
  \ using its built-in functions without the need for third-party libraries. Here's\
  \ a\u2026"
lastmod: '2024-03-13T22:44:59.767928-06:00'
model: gpt-4-0125-preview
summary: Elixir provides a straightforward way to capitalize strings using its built-in
  functions without the need for third-party libraries.
title: Capitalizing a string
weight: 2
---

## How to:
Elixir provides a straightforward way to capitalize strings using its built-in functions without the need for third-party libraries. Here's a simple example:

```elixir
string = "elixir programming"
capitalized_string = String.capitalize(string)
IO.puts capitalized_string
```

Output:

```
Elixir programming
```

For cases where more control or complex capitalization logic is needed, you might combine different String functions. For instance, if you want to capitalize every word in a sentence, you can split the sentence into words, capitalize each, and then join them back together:

```elixir
sentence = "elixir is fun"
capitalized_sentence = sentence 
                        |> String.split() 
                        |> Enum.map(&String.capitalize/1) 
                        |> Enum.join(" ")

IO.puts capitalized_sentence
```

Output:

```
Elixir Is Fun
```

While Elixir’s standard library covers most needs, for more nuanced text manipulation, including advanced string capitalization, you might explore third-party libraries such as Cldr for internationalization, which can offer locale-specific capitalization behaviors.
