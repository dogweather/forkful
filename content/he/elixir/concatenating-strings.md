---
title:                "שרשור מחרוזות"
date:                  2024-01-20T17:34:39.966235-07:00
model:                 gpt-4-1106-preview
simple_title:         "שרשור מחרוזות"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? מה ולמה?
Concatenating strings means sticking them together end-to-end. Programmers do this to build up text dynamically or to combine data into a readable format.

## How to: איך לעשות זאת
In Elixir, we glue strings together using the `<>` operator. Here are a few examples:

```elixir
name = "אליס"
greeting = "שלום, " <> name <> "!"
IO.puts greeting # "שלום, אליס!"
```

Also for variables:

```elixir
first_name = "אליס"
last_name = "המלך"
full_name = first_name <> " " <> last_name
IO.puts full_name # "אליס המלך"
```

Combining strings and variables:

```elixir
base = "הקוד שלך: "
code = "123ABC"
message = base <> code
IO.puts message # "הקוד שלך: 123ABC"
```

## Deep Dive עיון מעמיק
String concatenation in Elixir is straightforward due to its UTF-8 support, which means it handles Hebrew and other languages seamlessly. Historically, programmers juggled different encodings, which often led to bugs and headaches. Elixir avoids this complexity with its binary-based strings.

Alternatives to `<>` include string interpolation for variables insertion:

```elixir
name = "אליס"
IO.puts "שלום, #{name}!" # "שלום, אליס!"
```

Interpolation is handier when you need to insert variables within strings, while concatenation keeps things simple when you're just joining strings end-to-end.

Implementation-wise, Elixir's concatenation doesn't suffer from the "Schlemiel the Painter's algorithm" problem that languages like Java did, where strings are immutable, and concatenation creates new strings, leading to inefficiency. Elixir strings, based on Erlang's binaries, are efficiently handled in the VM, making concatenation fast.

## See Also ראה גם
- Elixir's official documentation on strings: [https://hexdocs.pm/elixir/String.html](https://hexdocs.pm/elixir/String.html)
- For a deeper understanding of binaries and strings in Elixir: [https://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html](https://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html)
- An exploration of pattern matching with strings: [https://elixirschool.com/en/lessons/basics/pattern-matching/](https://elixirschool.com/en/lessons/basics/pattern-matching/)
