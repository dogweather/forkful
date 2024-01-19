---
title:                "Converting a string to lower case"
html_title:           "Gleam recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?
Converting a string to lower case means changing all the uppercase characters in a string to their lower case equivalents. It's a regular operation when comparing string data where case-sensitivity can lead to incorrect comparisons.

## How to:
Here's a small chunk of Gleam code implementing this operation.
```Gleam
import gleam/string

pub fn lower_case_string(word: String) -> String {
  string.lower(word)
}
```

When running this function:
```Gleam
let result = lower_case_string("HELLO, WORLD!")
assert result == "hello, world!"
```
We'll get `"hello, world!"` as a result.

## Deep Dive
Converting strings to lower case is as old as computer programming itself. It's often vital when comparing or searching strings. For example, when comparing a user input like "EMAIL" with a stored value, "Email", they won't match unless you lower both to "email".

Gleam deals elegantly with this common operation using the `string.lower` function. However, you could build it from scratch iterating over characters of the string and replacing uppercase characters with their lower version.

Alternatives are few, because the crux of the operation essentially remains the same despite language or platform. The implementation may differ in internals, efficiency, or method, but the objective stays constant: ensure string comparisons are case-insensitive.

## See Also
- Gleam's `gleam/string` library documentation at "https://hexdocs.pm/gleam_stdlib/gleam/string"
- ASCII Table for a full list of characters and their codes at "http://www.asciitable.com/"
- String comparison in the Gleam Book at "https://gleam.run/book/tour/basic-types.html#comparing"