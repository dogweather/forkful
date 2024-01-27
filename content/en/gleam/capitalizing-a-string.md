---
title:                "Capitalizing a string"
date:                  2024-01-19
html_title:           "C recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Capitalizing a string means making the first letter uppercase while the rest stay lowercase. Programmers do this to standardize data inputs, for stylistic purposes, or to conform to grammatical rules in user interfaces.

## How to:

In Gleam, we can define a function to capitalize a string. Currently, Gleam's standard library doesn't directly provide a capitalize function, so we roll our own using string slicing:

```gleam
import gleam/string

pub fn capitalize(text: String) -> String {
  let head = string.slice(text, 0, 1)
  let tail = string.slice(text, 1, string.len(text))
  
  string.append(string.uppercase(head), string.lowercase(tail))
}

pub fn main() {
  assert capitalize("hello") == "Hello"
  assert capitalize("WORLD") == "World"
}
```

Sample output:

```plaintext
"Hello"
"World"
```

## Deep Dive

Historically, the function to capitalize strings is often included in the standard libraries of many languages. However, Gleam, being a young language, might lack certain convenience functions, leaving it to developers to implement them. Alternatives for capitalizing strings can involve using regular expressions or unicode libraries for more complex cases that consider language-specific rules. In our basic example, the implementation details are simple: we divide the string into two parts (head and tail), capitalize the first letter, and recombine them.

## See Also

- Unicode text segmentation for more complex rules: [https://unicode.org/reports/tr29/](https://unicode.org/reports/tr29/)
