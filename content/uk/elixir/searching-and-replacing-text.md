---
title:                "Пошук та заміна тексту"
aliases:
- uk/elixir/searching-and-replacing-text.md
date:                  2024-01-20T17:57:48.897118-07:00
model:                 gpt-4-1106-preview
simple_title:         "Пошук та заміна тексту"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Що і чому?
Searching and replacing text means finding specific sequences of characters in a string and swapping them with something else. Programmers do this for tasks like correcting errors, changing variable names, or updating data across multiple files.

## Як це зробити:
```elixir
original_text = "The quick brown fox jumps over the lazy dog"
search_pattern = "lazy"
replacement = "energetic"

fixed_text = String.replace(original_text, search_pattern, replacement)

IO.puts fixed_text
```
Output:
```
The quick brown fox jumps over the energetic dog
```

## Занурення у глибину
Originally, searching and replacing text was a feature in text editors, evolving as a handy tool for programming. Elixir's `String.replace/3` function makes it straightforward, but Regex can be used when more complex patterns are involved. Alternatives include `String.replace_leading/3` or `String.replace_trailing/3` for more specific use cases. Under the hood, the `String` module handles Unicode-compliant character data, which means your search and replace operations are reliable across various languages and special characters.

## Див. також
- Elixir's official documentation for the `String` module: [hexdocs.pm/elixir/String.html](https://hexdocs.pm/elixir/String.html)
- Regular Expressions in Elixir: [hexdocs.pm/elixir/Regex.html](https://hexdocs.pm/elixir/Regex.html)
- Practical Elixir programming tips: [elixirschool.com/en](https://elixirschool.com/en)
