---
title:                "Finding the length of a string"
html_title:           "Arduino recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Finding the length of a string refers to counting the number of characters (including spaces) present in a text string. Programmers do it quite often because it helps in many operations like validation, manipulation, and comparison of the string data.

## How to:

Finding the length of a string in Elixir is straightforward. You can do it by using the `String.length()` function. Here's an example:

```elixir
IO.puts String.length("Hello, world!")
```

This will output:

```elixir
12
```

Remember, it counts spaces too. Hence, the length of `"Hello, world!"` is shown as 12.

## Deep Dive:

Historically, Elixir treats strings as binaries. In particular, Elixir uses UTF-8 to encode its strings, which supports a wide range of characters including emojis. This is why in Elixir, the `String.length/1` function might not always behave the way you'd expect with multi-byte characters.

Consider:

```elixir
IO.puts String.length("é")
```

It returns `1`, because "é" is a single character, even though it's composed of two bytes in UTF-8.

As an alternative, you could count bytes instead:

```elixir
IO.puts byte_size("é")
```

This returns `2`, as "é" takes up two bytes in UTF-8.

The `String.length/1` in Elixir does not always correspond to the underlying bytes. That's because Elixir counts graphemes, not bytes, when calculating string length.

## See Also:

For more information, refer to the following resources:

- Elixir official doc: [String](https://hexdocs.pm/elixir/String.html)
- Unicode Standard: [UTF-8, UTF-16, UTF-32 & BOM](http://www.unicode.org/faq/utf_bom.html)
- Practical Elixir blog post: [Elixir strings and binary data](https://pragtob.wordpress.com/elixir-strings-and-binary-data/)