---
title:                "Finding the length of a string"
html_title:           "Gleam recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Finding the length of a string refers to determining the number of characters (including spaces and punctuation) contained in a text string. Programmers often do it to control text data, validate inputs, or allocate memory.

## How to:

Here's how you find the length of a string in Gleam:

```Gleam
import gleam/string.{length}

fn main() {
  let text_string = "Hello, Gleam!"
  let str_length = length(text_string)
  print(str_length)
}
```

When you run this program, you should see the following output:

```Gleam
13
```

## Deep Dive

- **Historical context**: String length determination is not a new concept; it's a standard function present in almost every programming language, each implemented based on the language's design principles.

- **Alternatives**: While most languages, including Gleam, provide built-in functions for length determination, an alternative manual approach involves iterating through the string until the end is reached and counting the iterations.

- **Implementation details**: Gleam’s `length` function internally utilizes the Erlang `byte_size` function, which returns the length of a string in bytes. However, in UTF-8 (which Gleam uses for strings), it's important to note that a single character can span multiple bytes, depending on its complexity. 

## See Also

For a deeper understanding of strings in Gleam, check out these resources:

- [Gleam `string` module documentation](https://hexdocs.pm/gleam_stdlib/gleam/string/)
- [Erlang `byte_size` function](https://erlang.org/doc/man/erlang.html#byte_size-1)
- [Understanding UTF-8](https://en.wikipedia.org/wiki/UTF-8)