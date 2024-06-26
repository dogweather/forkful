---
date: 2024-01-20 17:38:20.166106-07:00
description: 'How to: Elixir makes it a breeze. Use the `String.downcase/1` function.'
lastmod: '2024-03-13T22:44:59.771275-06:00'
model: gpt-4-1106-preview
summary: Elixir makes it a breeze.
title: Converting a string to lower case
weight: 4
---

## How to:
Elixir makes it a breeze. Use the `String.downcase/1` function:

```elixir
original = "LoReM IPSUM"
lowercased = String.downcase(original)

IO.puts original
IO.puts lowercased
```

Output:

```
LoReM IPSUM
lorem ipsum
```

## Deep Dive
Elixir's string handling is Unicode aware, which matters a lot for proper lower-casing across different alphabets and scripts. Historically, string manipulation in programming languages didn't always account for this complexity. 

Before Elixir's current approach, some older languages offered simplistic methods that might work fine for English but would trip on languages like Turkish, where, for instance, an uppercase 'i' does not become 'I' but rather 'İ'.

Internally, Elixir uses Unicode's case mapping to get this right. And there are alternatives; for example, `String.downcase/2` allows you to specify a locale, which comes in handy for language-specific behaviors.

```elixir
turkish = "GÖLCÜK"
String.downcase(turkish, :tr)
```

Output:

```
gölcük
```

In the example above, notice how the 'I' character is preserved appropriately according to Turkish casing rules.

## See Also
- Elixir's official `String` module documentation: https://hexdocs.pm/elixir/String.html
- Unicode case mapping: https://www.unicode.org/reports/tr21/tr21-5.html
- A quick guide to Unicode in Elixir: https://elixir-lang.org/blog/2017/01/05/elixir-and-unicode-part-1/
