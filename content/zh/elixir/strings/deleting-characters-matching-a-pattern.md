---
date: 2024-01-20 17:41:49.786516-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C\uFF1A) Elixir provides the `String`\
  \ module which contains functions to handle string manipulations. Here's a simple\
  \ example of deleting\u2026"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.334146-06:00'
model: gpt-4-1106-preview
summary: Elixir provides the `String` module which contains functions to handle string
  manipulations.
title: "\u5339\u914D\u6A21\u5F0F\u5220\u9664\u5B57\u7B26"
weight: 5
---

## How to: (如何操作：)
Elixir provides the `String` module which contains functions to handle string manipulations. Here's a simple example of deleting characters:

```elixir
# 删除字符串中所有的数字
string = "My phone number is 123-456-7890."
clean_string = String.replace(string, ~r/\d+/, "")
IO.puts(clean_string)
# Output: My phone number is --.
```

Or if you want to strip whitespace from a string, you can do this:

```elixir
# 删除字符串前后的空格
string = "   Hello, World!   "
trimmed_string = String.trim(string)
IO.puts(trimmed_string)
# Output: Hello, World!
```

## Deep Dive (深入了解)
Removing characters matching a pattern in Elixir is often done using regular expressions (regex). Regex is a powerful pattern-matching language that's been around for decades. While Elixir's `String` module is quite robust, alternatives such as `Regex` module or list comprehensions could also be used for more complex patterns.

The Elixir standard library leverages the high performance of Erlang's underlying regular expression library, which is based on Perl's regular expression library. This makes operations like matching and replacing highly efficient in Elixir.

Besides performance considerations, thoughtfully choosing regexp patterns is crucial as poorly designed patterns can lead to inefficient or even incorrect code.

## See Also (另请参阅)
- Elixir's `String` module documentation: https://hexdocs.pm/elixir/String.html
- `Regex` module documentation: https://hexdocs.pm/elixir/Regex.html
- Interactive Elixir regex tool: https://regexr.com/ - While it's not Elixir-specific, it's helpful for testing regex patterns that can be used in Elixir.
