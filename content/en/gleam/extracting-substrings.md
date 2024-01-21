---
title:                "Extracting substrings"
date:                  2024-01-20T17:45:42.557148-07:00
model:                 gpt-4-1106-preview
simple_title:         "Extracting substrings"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?
Extracting substrings is like plucking a cherry from the cake; you pick a specific piece from a string. Programmers do it to isolate, analyze, or manipulate smaller chunks of data from larger text sequences.

## How to:
Gleam's got a handful of ways to deal with substring extraction. Below are some examples:

```gleam
import gleam/string

let story = "Learning Gleam is fun!"

// Pulling out "Gleam" from the string
let gleam = string.slice(story, 9, 14)
assert gleam == Ok("Gleam")

// Grabbing "fun!" using a negative index
let fun = string.slice(story, -4, -1)
assert fun == Ok("fun!")

// If the indices are out of bounds, we get an error
let oops = string.slice(story, 30, 40)
assert oops == Error(Nil)
```
The `slice` function is the go-to for getting substrings. Provided indices are inclusive at the start and exclusive at the end. Negative indices count from the end.

## Deep Dive
Substring extraction isn't new; it's as old as the hills in programming. In days of yore, languages like C made you jump through hoops with pointers to snatch substrings. Gleam and modern languages simplify the task with built-in functions, like `slice`.

Alternatives? Sure. You could use pattern matching to dissect strings, but `slice` is slicker for simple extractions.

Implementation-wise, `slice` needs to consider string encoding, like UTF-8. It ensures characters, not just bytes, get extracted correctly without garbling multi-byte characters. This wasn't a picnic back in the ASCII-only era.

## See Also
- If you're feeling nostalgic or just plain curious, take a gander at how C tackled strings with pointers: [C String handling](https://en.cppreference.com/w/c/string/byte)