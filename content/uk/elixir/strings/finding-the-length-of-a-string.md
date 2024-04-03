---
date: 2024-01-20 17:47:03.420251-07:00
description: "(\u0429\u043E \u0456 \u0447\u043E\u043C\u0443?) Finding the length of\
  \ a string means counting the number of characters it contains. Programmers do this\
  \ to validate input, control loops, or\u2026"
lastmod: '2024-03-13T22:44:48.707860-06:00'
model: gpt-4-1106-preview
summary: "(\u0429\u043E \u0456 \u0447\u043E\u043C\u0443."
title: "\u0412\u0438\u0437\u043D\u0430\u0447\u0435\u043D\u043D\u044F \u0434\u043E\u0432\
  \u0436\u0438\u043D\u0438 \u0440\u044F\u0434\u043A\u0430"
weight: 7
---

## What & Why?
(Що і чому?)

Finding the length of a string means counting the number of characters it contains. Programmers do this to validate input, control loops, or work with data structures.

## How to:
(Як зробити:)

In Elixir, we use the `String.length/1` function. Here's how:

```elixir
my_string = "Привіт, світ!"
length = String.length(my_string)
IO.puts(length)
```

Output:

```
13
```

## Deep Dive
(Поглиблене занурення:)

Finding the length of a string is straightforward but it's worth noting that Elixir handles Unicode correctly, counting graphemes, not bytes. Historically, not all programming languages did this, which led to inaccurate lengths with non-ASCII characters.

For alternatives, we might consider `byte_size/1`, but this measures raw bytes, not characters, so it's different.

Understanding the implementation, Elixir's usage of the BEAM VM means it's built with Unicode awareness from the ground up, ensuring accurate string length measurements regardless of the language used.

## See Also
(Див. також:)

- Elixir's official docs on strings: https://hexdocs.pm/elixir/String.html
- Unicode in Elixir: https://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html
- Graphemes vs. bytes discussion: http://jozefg.bitbucket.io/posts/2015-01-29-unicode.html
