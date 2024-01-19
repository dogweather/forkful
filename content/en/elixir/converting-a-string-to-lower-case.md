---
title:                "Converting a string to lower case"
html_title:           "Elixir recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?

Converting a string to lowercase means changing all alphabetic characters in the string to their lowercase equivalent. Programmers do this to normalize data for processes like searching and sorting.

## How to

Here's a simple one-liner in Elixir to convert a string to lowercase:

```elixir
String.downcase("Bring It On DOWN!")
```
When executed, the output would be:

```elixir
"bring it on down!"
```

It's a straight shooter, no fuss.

## Deep Dive

Elixir’s String `downcase/1` function goes way back to the early days of functional programming. In fact, it leverages Erlang's `unicode:characters_to_lower/1` under its hood.

Alternatives? Well, you could always roll your own recursive function, like so:

```elixir
defmodule MyString do
  def downcase(<< char :: utf8, rest :: binary >>) when char in ?A..?Z do
    << char + 32 :: utf8 >> <> downcase(rest)
  end

  def downcase(<< _ :: utf8, rest :: binary >>), do: downcase(rest)
  def downcase(""), do: ""
end
```
But why reinvent the wheel? The `String.downcase/1` function is efficient and reads well. Also, pay attention to Unicode; it's a lot more than just ASCII.

**Be careful**, `downcase/1` function can't handle `nil` inputs, so ensure your string is not `nil` before calling it.

## See Also

- Elixir docs for `String.downcase/1`: [https://hexdocs.pm/elixir/String.html#downcase/2](https://hexdocs.pm/elixir/String.html#downcase/2)
- Older discussion on converting strings to lowercase in Elixir: [https://stackoverflow.com/questions/20956229/idiomatically-convert-string-to-lowercase](https://stackoverflow.com/questions/20956229/idiomatically-convert-string-to-lowercase)
- For the bold, lookup the Erlang docs on `unicode:characters_to_lower/1`: [http://erlang.org/doc/man/unicode.html#characters_to_lower-1](http://erlang.org/doc/man/unicode.html#characters_to_lower-1)