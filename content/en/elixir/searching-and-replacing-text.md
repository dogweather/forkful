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

## Why 

Searching and replacing text is a common task in programming, especially when dealing with large amounts of data or working on text-based projects. With the help of Elixir, this process can be made easier and more efficient, saving developers time and effort.

## How To 

To search and replace text in Elixir, we can use the `String.replace/4` function. This function takes in four arguments: the original string, the pattern to search for, the replacement text, and a number indicating the maximum number of matches to replace.

```

Elixir:

original_string = "Hello, world!"
pattern = "world"
replacement = "universe"

String.replace(original_string, pattern, replacement, 1)
# Output: "Hello, universe!"

If we want to replace all occurrences of the pattern, we can omit the fourth argument or pass in `:global` as the fourth argument.

```

original_string = "Hello, world! It's a beautiful world."
pattern = "world"
replacement = "universe"

String.replace(original_string, pattern, replacement)
# Output: "Hello, universe! It's a beautiful universe."

String.replace(original_string, pattern, replacement, :global)
# Output: "Hello, universe! It's a beautiful universe."

```

We can also use regular expressions as the pattern to make more complex replacements. For example, if we want to replace all numbers in a string with the word "number", we can do the following:

```

Elixir:

original_string = "I have 5 oranges and 10 apples."
pattern = ~r/\d+/
replacement = "number"

String.replace(original_string, pattern, replacement, :global)
# Output: "I have number oranges and number apples."

## Deep Dive

The `String.replace/4` function uses the `:binary` module under the hood, specifically the `:binary.replace/3` function. This allows for efficient and optimized searching and replacing of text in Elixir.

It's also worth noting that the `String.replace/4` function is not limited to just strings, it can also be used on binaries and lists of characters as well. This allows for more flexibility in handling different types of data.

## See Also 

- Official Elixir documentation for String.replace: https://hexdocs.pm/elixir/String.html#replace/4
- Elixir tutorial on pattern matching and regular expressions: https://elixirschool.com/en/lessons/basics/pattern-matching/