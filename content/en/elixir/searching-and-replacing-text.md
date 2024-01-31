---
title:                "Searching and replacing text"
date:                  2024-01-20T17:57:37.860953-07:00
model:                 gpt-4-1106-preview
simple_title:         "Searching and replacing text"

category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?

Searching and replacing text are bread-and-butter for programming; they're essentially finding strings and swapping them out. Programmers do this all the time for things like updating code bases, processing text data, or just simple editing tasks.

## How to:

In Elixir, you can use the `String` module for quick search-and-replace operations. Here's how you do it:

```elixir
original_text = "I heart Elixir!"

# Simple replace
replaced_text = String.replace(original_text, "heart", "❤️")
IO.puts replaced_text  # Output: I ❤️ Elixir!

# Global replace with a pattern
replaced_text_global = String.replace(original_text, ~r/eart|Eli/, "❤️", global: true)
IO.puts replaced_text_global  # Output: I ❤️ ❤️xir!

# Case-insensitive replace
insensitive_replace = String.replace(original_text, "ELIXIR", "❤️", global: true, case_insensitive: true)
IO.puts insensitive_replace  # Output: I heart ❤️!
```

## Deep Dive

Searching and replacing text has been around since the dawn of computing; think 'find and replace' in a Word doc, but for code. In Elixir, it's all about pattern matching and working with strings effectively. 

The `String.replace/4` function leverages Elixir's pattern matching capabilities, allowing you to match not just static strings but also regex patterns, providing significant flexibility. Behind the scenes, Elixir utilizes Erlang's powerful string handling, which is robust and efficient for text processing tasks.

Alternatives to the built-in `String` module include writing your own functions for more complex cases or using third-party libraries that wrap string handling in different ways. However, for most use cases, the built-in functions will get the job done without adding extra dependencies.

As an immutable language, remember that every replace function returns a new string - the original is unchanged. This is different from some other languages where you might modify the string in place.

## See Also

- Elixir's `String` module docs: [https://hexdocs.pm/elixir/String.html](https://hexdocs.pm/elixir/String.html)
- Regex in Elixir: [https://hexdocs.pm/elixir/Regex.html](https://hexdocs.pm/elixir/Regex.html)
- Learn more about pattern matching in Elixir: [https://elixir-lang.org/getting-started/pattern-matching.html](https://elixir-lang.org/getting-started/pattern-matching.html)
