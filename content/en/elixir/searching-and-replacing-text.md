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
Searching and replacing text is the process of finding and replacing specific words, phrases, or patterns within a text document or code file. Programmers often utilize this technique to make changes quickly and efficiently in their code, improving readability and eliminating errors.

## How to:
Coding examples:
```
# Replace a word with another word in a string
my_string = "Hello, world!"
replaced_string = String.replace(my_string, "world", "Elixir")

# case-sensitive searching and replacing
my_phrase = "Hello, Hello, hello"
replaced_phrase = String.replace(my_phrase, "Hello", "Hi", case: :smart)

# Replace a pattern using regular expressions
my_sentence = "This is a sentence, not a question."
replaced_sentence = Regex.replace(~r/question/, my_sentence, "statement")
```
Output:
```
"Hello, Elixir!" # for the first example
"Hi, Hi, hello" # for the second example
"This is a sentence, not a statement." # for the third example
```

## Deep Dive:
Searching and replacing text has been a common practice among programmers for decades. In the early days of computing, this task was often carried out manually, making it time-consuming and prone to errors. However, with the advancement of programming languages and powerful text editors, programmers can now search and replace text with a few simple commands.

There are several alternatives to the built-in functions in Elixir for searching and replacing text. For instance, the Erlang library provides the module `:regexp`, which allows for searching and replacing using regular expressions. Additionally, libraries such as `StringScanner` and `Timex` also offer options for searching and replacing text in various formats.

To implement the search and replace functionality, Elixir uses the `String.replace/3` and `Regex.replace/3` functions. Both functions take three arguments - the string or pattern to replace, the string to replace it with, and any options such as case sensitivity or regular expression flags.

## See Also:
- [Elixir docs on String replace](https://hexdocs.pm/elixir/String.html#replace/3)
- [Erlang docs on regular expressions](http://erlang.org/doc/man/regexp.html)
- [Tutorial on using regular expressions in Elixir](https://www.bignerdranch.com/blog/elixir-regex-tutorial/)