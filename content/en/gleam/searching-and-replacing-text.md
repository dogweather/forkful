---
title:                "Searching and replacing text"
html_title:           "Gleam recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?
Searching and replacing text in programming is about finding specific strings in a block of data and replacing them with a new set. Programmers use this to update or sanitize data, fix errors, and refactor code.

## How to:

You can use `search` function to find a specific string. Let's try it:
```Gleam
import gleam/string
let my_text = "Hello, World!"
string.contains(my_text, "World")
```
Running this will return `True`, indicating the "World" string exists in `my_text`.

Replacing strings is as easy:
```Gleam
import gleam/string
let my_text = "Hello, World!"
string.replace(my_text, "World", "Gleam")
```
After running this, `my_text` will now be "Hello, Gleam!"

## Deep Dive

Text searching and replacing is among the earliest and most basic functionalities in computing. Early editors and programming languages had these features built in.

Gleam's `gleam/string` functions operate at a pretty basic level. There are a number of more sophisticated methods for searching and replacing text, such as regular expressions, which can use pattern matching to find and replace text.

Internally, text searching in Gleam uses the Knuth-Morris-Pratt (KMP) algorithm while the `replace` function is implemented with the faster Boyer–Moore string-search algorithm.

## See Also
Check out: 
- [Erlang string module](http://erlang.org/doc/man/string.html) for more information on underlying implementation.
- [Gleam/string docs](https://gleam.run/stdlib/string/) for more ado about strings in Gleam.
- [Regular expressions in Gleam](https://awesomeopensource.com/project/gleam-lang/stdlib) if you want more advanced search-and-replace methods.