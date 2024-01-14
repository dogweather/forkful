---
title:                "Elixir recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why
As a functional programming language, Elixir provides efficient and powerful ways to manipulate and analyze data. One common task in programming is finding the length of a string. In this blog post, we will explore how to do that in Elixir and the benefits it brings.

## How To
Finding the length of a string may seem like a basic task, but with Elixir’s built-in functions, it becomes even easier. Let’s look at a simple example:

```Elixir
string = "Hello, world!"
length = String.length(string)
IO.puts(length)
```

In this example, we assign the string "Hello, world!" to a variable called `string`. Then, we use the `String.length()` function to calculate the length of the string and store it in a variable called `length`. Finally, we print the length using the `IO.puts()` function. Running this code will output `13`, which is the length of the string.

But what if the string is empty? Will `String.length()` still work? Let’s try it out:

```Elixir
empty_string = ""
length = String.length(empty_string)
IO.puts(length)
```

Surprisingly, the code still outputs `0`, which is the length of an empty string. This is because Elixir treats an empty string as a list with no elements, and the `length()` function simply counts the number of elements in a list.

Another interesting feature of Elixir is that it allows Unicode characters in strings and still provides an accurate length. For example, let’s try finding the length of a string containing emojis:

```Elixir
string = "👋 Hello, 🌎!"
length = String.length(string)
IO.puts(length)
```

The output of this code will be `12`, even though there are emojis in the string. This is because Elixir handles each Unicode character correctly when calculating the length of a string.

## Deep Dive
Now that we have seen some examples of finding the length of a string in Elixir, let’s take a deeper look at how it works. In Elixir, strings are actually represented as binaries, which are data types that contain sequences of bytes. But when we use the `String.length()` function, it counts the number of graphemes (visible characters) instead of bytes. This is why it works accurately even with Unicode characters.

It is also worth mentioning that Elixir’s string functions are heavily optimized for performance. So even if you have a very long string, calculating its length will still be fast and efficient.

## See Also
- [Elixir String Module](https://hexdocs.pm/elixir/String.html)
- [Elixir Unicode and UTF-8](https://elixir-lang.org/getting-started/unicode-char-lists-and-utf-8.html)

Now that you know how to efficiently find the length of a string in Elixir, try it out for yourself in your next project. Utilizing Elixir’s powerful features can bring great benefits to your code. Happy coding!