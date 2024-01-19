---
title:                "Finding the length of a string"
html_title:           "Elixir recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Finding the length of a string means determining the number of characters in that string. As programmers, we do this to handle control flow, validate input, and manipulate string data efficiently. 

## How to:

In Elixir, you can easily find the length of a string by using the `String.length/1` function. Here's a quick demonstration:

```elixir
IO.puts String.length("Hello, Elixir!")  # Outputs: 13
```

That's it! You have now found the length of the string "Hello, Elixir!" which happens to be 13. 

## Deep Dive

Historically, there's been a good reason for knowing a string's length. In early languages like C, the end of a string was marked by a null character. Knowing the string's length helped avoid reading past its end. 

Elixir introduced more ergonomic options like the `String.length/1` function. This function works by directly counting the graphemes in a string, which are what we might consider 'letters'. This accounts for all Unicode sequences, making it vastly more precise for international use.

It's important to note that because of Wide Characters and Unicode, the result might not be an exact match to the bytesize of the string, as a single character might take more than one byte. For example:

```elixir
IO.inspect(String.bytesize("Hello, 世界"))  # Outputs: 13
IO.inspect(String.length("Hello, 世界"))   # Outputs: 9
```

Although there are other ways to calculate the length of a string in Elixir (like using the `byte_size/1` function), `String.length/1` is the go-to method due to its precision and efficient handling of Unicode strings.

## See Also:

For more on Elixir strings and UTF-8-encoded strings:
1. [Elixir Doc on Strings](https://hexdocs.pm/elixir/String.html)
2. [The Elixir programming language website](https://elixir-lang.org/)
3. [Understanding Unicode and UTF-8 in Elixir](https://www.jungledisk.com/blog/2017/06/22/understanding-unicode-in-elixir-part-1-length/)