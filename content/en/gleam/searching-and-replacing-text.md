---
title:                "Searching and replacing text"
html_title:           "Arduino recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Gleam Programming: Searching and Replacing Text

## What & Why?
Searching and replacing text refers to finding a particular piece of text in a document or data stream and substituting it with something else. Programmers routinely use this approach to update or modify data, fix errors, or rearrange text structures.

## How to:
To search and replace text in Gleam, we primarily use the `replace` function from the `gleam/string` module.

```Gleam
import gleam/string

let old_text = "I love coffee"
let new_text = string.replace(old_text, "coffee", "tea")
assert new_text == "I love tea"
```

This code replaces the word "coffee" with "tea". Simple, isn't it?

## Deep Dive
Searching and replacing text have been staples of text processing since early computing. This method, widely used in various programming languages, allows convenient, reliable, and fast text transformations.

There are alternative ways to perform search and replace in Gleam, such as using regex pattern matching. This is usually more complex and intense but provides more flexibility when dealing with more complicated patterns.

In Gleam's implementation, the `replace` function leverages Erlang's :binary module functions. Its performance is directly tied to that of Erlang's binary functions.

## See Also
To learn more about searching and replacing text in Gleam and other functionality the language offers, check out the following links:

- Gleam's official documentation: <https://gleam.run/docs/>
- String module documentation: <https://hexdocs.pm/gleam_stdlib/gleam/string>
- Guide to pattern matching in Gleam: <https://gleam.run/tour/pattern-matching/>