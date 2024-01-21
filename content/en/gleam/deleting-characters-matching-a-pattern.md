---
title:                "Deleting characters matching a pattern"
date:                  2024-01-20T17:42:12.349122-07:00
model:                 gpt-4-1106-preview
simple_title:         "Deleting characters matching a pattern"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why?
Deleting characters matching a pattern is about finding specific sequences in text and removing them. Programmers do this to clean data, parse relevant information, or sanitize inputs.

## How to:
In Gleam, you usually work with the `String` module for text manipulation. Regex isn't built-in, but you can remove fixed patterns or use external libraries for more complex tasks. Let's clean up some text by removing exclamation marks from a string.

```gleam
import gleam/string

pub fn remove_exclamations(text: String) -> String {
  string.replace(text, "!", "")
}

// Usage
fn main() {
  let cleaned_text = remove_exclamations("Hello, World!!")
  assert cleaned_text == "Hello, World"
}
```
This code replaces all exclamation marks with an empty string, effectively removing them.

## Deep Dive
Gleam is a statically typed language for the Erlang virtual machine, favoring performance and reliability. Its String module provides basic manipulation functions but lacks advanced pattern matching found in regex libraries.

For historical context, regex has been around since the 1950s, originating from formal language theory and automata theory. Most programming languages adopted some form of regex implementation for pattern matching.

In Gleam, to handle more sophisticated pattern deletions, you'd typically reach for an Erlang library or an Elixir module through interop since Gleam's ecosystem is still young. The implementation relies on the robustness of the BEAM (Erlang virtual machine) and its longstanding libraries.

Alternatives in Gleam might include writing your own pattern matching functions for more predictable patterns or handling specific cases with `String` functions like `slice`, `split`, or `trim`.

## See Also
- An introduction to Regex in general (not Gleam-specific): [https://www.regular-expressions.info/](https://www.regular-expressions.info/)