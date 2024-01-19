---
title:                "Capitalizing a string"
html_title:           "Gleam recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Capitalizing a string means changing the first letter of the string to upper case. Programmers do this to make text more readable or to meet certain data formatting requirements.

## How to:

There is no built-in function to capitalize a string in Gleam (as of current version). Here's a hand-rolled version :

```gleam
import gleam/string

fn capitalize(s: String) -> String {
  case string.split_at(s, 1) {
    [] -> s
    [first | rest] -> string.concat([string.to_upper(first), string.concat(rest)])
  }
}

let example = capitalize("hello, world!")
assert example == "Hello, world!"
```
When run, the sample script will output a capitalized "Hello, world!".

## Deep Dive

The capitalize function example splits the string into a list of characters, converts the first character to uppercase, and then joins them back into a string. The history of string capitalization in any programming language is a mix of built-in functions and tricks like this. If Gleam introduces a built-in capitalize function in the future, it will likely implement it in a similar way.

Different languages have different built-in functions to capitalize a string - JavaScript has `toUpperCase()`, Python has `.capitalize()`, and Ruby has `.capitalize()` among many others. Gleam falls back on `split_at()`, `to_upper()`, and `concat()` for this task.

## See Also

For an overview about text handling in Gleam, see the official Gleam String documentation at: https://hexdocs.pm/gleam_stdlib/gleam/string.

For the detailed explanation of `split_at()`, `to_upper()`, and `concat()`, visit https://hexdocs.pm/gleam_stdlib/gleam/string.html#split_at2, https://hexdocs.pm/gleam_stdlib/gleam/string.html#to_upper1, and https://hexdocs.pm/gleam_stdlib/gleam/string.html#concat1, respectively.

Running the code samples locally requires Gleam to be installed. You can find setup instructions on the official website: https://gleam.run/getting-started/.