---
title:                "Converting a string to lower case"
date:                  2024-01-20T17:38:13.472927-07:00
model:                 gpt-4-1106-preview
simple_title:         "Converting a string to lower case"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?
Converting a string to lowercase means transforming all uppercase characters to their lowercase counterparts. Programmers do it for consistency, especially for comparison operations or to standardize user input data.

## How to:
In Gleam, string manipulation is straightforward. Use the `string.lowercase` function to convert a string to lowercase. Here’s a simple example:

```gleam
import gleam/string

pub fn demo() {
  let my_string = "Hello, World!"
  string.lowercase(my_string)
}

// This will output: "hello, world!"
```

## Deep Dive
Before the sanity of Unicode, we had ASCII, where converting to lowercase was a bit of math. Add 32 to any uppercase ASCII character code, and voilà – lowercase. With Unicode, it's trickier; the rules are complex and language-specific. In Gleam, all the heavy lifting is hidden away, giving you a simple function that works consistently across different languages.

Alternatives to string.lowercase could involve manually mapping over each character in a string, checking if it's uppercase, and then converting it. It's doable but why reinvent the wheel?

Under the hood, a function like `string.lowercase` likely relies on the underlying Erlang system's unicode libraries, which include robust support for case mapping in different languages. This is an incredibly complex topic because the idea of case doesn't even apply to a large number of writing systems.

## See Also
- Unicode Case Mapping FAQ: [http://www.unicode.org/faq/casemap_charprop.html](http://www.unicode.org/faq/casemap_charprop.html)
- Erlang's unicode module for a peek under the hood: [https://www.erlang.org/doc/man/unicode.html](https://www.erlang.org/doc/man/unicode.html)
