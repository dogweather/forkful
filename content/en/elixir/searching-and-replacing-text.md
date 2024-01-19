---
title:                "Searching and replacing text"
html_title:           "Elixir recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?
Searching and replacing text is a basic operation where we look for a particular sequence of characters (search) in a data structure and alter that sequence with new characters (replace). Programmers do it all the time for purposes like data manipulation, refactoring code, or even routine tasks like renaming variables for clarity.

## How to:
Here's how you can perform the search and replace operation in Elixir:

```Elixir
defmodule SearchingReplacing do
  def replace(text, dict) do
    Enum.reduce(dict, text, fn {k, v}, acc -> String.replace(acc, k, v) end)
  end
end

IO.puts SearchingReplacing.replace("Hello, Mars", %{"Mars" => "World"})
```

When you run the code, you get the following output:

```Shell
Hello, World
```

What this code does is creating a module with a function `replace`. The function accepts two arguments - the text where you want perform the search and replace operation, and a dictionary which maps the search terms to the replace terms.

## Deep Dive
Historically, the concept of search and replace originated from word processing software that needed a way to perform bulk alterations. Its importance was soon realized in programming as well, paving the way for its implementation in various languages, Elixir included.

As for alternatives, you might use the `:binary.replace/3` function which is more low-level and could offer performance benefits in some situations. Another way is to use the Regex module, which provides powerful functions for string matching and replacing, but requires a good understanding of regular expressions.

The underlying implementation of `String.replace/3` in Elixir is interesting. It involves splitting the original string into a list of parts, forming a new list with replaced strings where the search term matches, and then joining this list back into a single string, all achieved with the help of Elixir's efficient handling of lists.

## See Also
For a complete understanding, you should explore:

1. [Elixir Lang Documentation](https://hexdocs.pm/elixir/String.html#replace/3)
2. [Explain Elixir – String](https://www.erlang-solutions.com/blog/explain-elixir-string-module.html)
3. [Elixir School – Binaries, strings and charlists](https://elixirschool.com/en/lessons/advanced/binaries/)
4. [Regex in Elixir (Elixir Lang Guide)](https://elixir-lang.org/getting-started/regex.html)