---
title:                "Finding the length of a string"
html_title:           "Arduino recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Finding the length of a string refers to counting how many characters are in a text block. Programmers do this to enforce size restrictions, index correctly, or measure content.

## How to:

In Gleam, you'd find a string's length with the `len` method.

Here's an example:

```Gleam
import gleam/string

fn main() {
  let a_string = "Hello, Gleam!"
  string.len(a_string) // Returns 13
}
```

When executed, this will output `13` as "Hello, Gleam!" include 13 characters.

## Deep Dive

Finding the length of a string has been a basic operation since the early days of programming. Historical context surrounds it in terms of memory allocations and buffer overflows, resolved since languages like Gleam handle memory allocation automatically.

Alternatives? Sure, you can count characters manually looping through the string. But `string.len` is simpler and faster.

Behind the scenes, `string.len` uses embedded binary functions of Erlang (on which Gleam runs) to find a string's length. So, it's safe and efficient.

## See Also

Gleam language guide on string: (https://gleam.run/book/tour/strings.html).

Explore more about Erlang's binary operations: (http://erlang.org/doc/efficiency_guide/binaryhandling.html).