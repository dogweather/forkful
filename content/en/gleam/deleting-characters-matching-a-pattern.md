---
title:                "Deleting characters matching a pattern"
html_title:           "Lua recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why?

Deleting characters matching a pattern is a way of removing specific symbols from a string of text. It's a common task for programmers when cleaning or preparing data for further processing.

## How to:

Let's get right to it. We'll use a function to delete all the vowels from a string.

```Gleam
import gleam/string

fn delete_vowels(input_string: String) -> String {
  string.replace(input_string, "aeiou", "")
}
```

You can call this function as follows:

```Gleam
delete_vowels("Hello, Gleam!")
```

This will produce the following output:

```Gleam
"Hll, Glm!"
```

## Deep Dive:

The concept of pattern matching traverses a long history of programming, originating from primitive text processing methods. In Gleam, string manipulation is achieved through the `gleam/string` module, a symbol of the language's focus on functional programming principles. 

Alternative methods to character deletion exist. They range from iterative removal to using regular expressions. Yet, the `string.replace` function offers simplicity and readability, making it an ideal pick for many situations. 

In terms of implementation, `string.replace` scans the input for occurrences of the pattern and replaces them with the provided replacement (an empty string in our case). It's quick, efficient, and handles potentially large datasets quite well.

## See Also:

1. [Gleam String Module Docs](https://hexdocs.pm/gleam_stdlib/gleam/string.html)
   
2. [Gleam Programming Language Home](https://gleam.run)
   
   
There you have it! Happy Gleaming!