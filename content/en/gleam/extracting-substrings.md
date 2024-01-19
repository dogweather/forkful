---
title:                "Extracting substrings"
html_title:           "Gleam recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

# Gleaming Brighter with Substrings in Gleam

## What & Why?

Substring extraction? It's grabbing a portion of a string, like cherry-picking just the characters you need. Programmers do it for tasks like parsing input or manipulating text data.

## How to:

In Gleam, you can slice a string with `slice`. Here's how:

```Gleam
let s = "Hello, Gleam!"
let subs = s.slice(7, 12)
// Output: "Gleam"
```

So easy. Just use the start and end indices to frame your desired piece.

## Deep Dive

Interesting tidbits: Gleam's `slice` function wasn't always around! It's introduced in Gleam v0.14.0, inspired by Rust's slicing feature. Before its debut, Gleamers had to do more cumbersome character iterating to extract substrings! Such advancements are excellent at making coding life easier for programmers.

Alternative? There are functions like `split`, but in certain scenarios, `slice` performance is superior. Substrings of great lengths are one such instance.

If you're wondering how `slice` functions internally, it uses `binary.part` from Erlang's binary module. It's quick, safe, and efficient.

## See Also

For more about string manipulation in Gleam, buckle up and go here:
- [Gleam Docs: Strings and Characters](https://gleam.run/book/tour/strings-characters.html)
- [Slice function in Gleam: Github](https://github.com/gleam-lang/gleam/pull/1237)

Continue gleaming! Happy coding y'all.