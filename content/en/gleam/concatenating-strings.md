---
title:                "Concatenating strings"
date:                  2024-01-20T17:34:31.833203-07:00
model:                 gpt-4-1106-preview
simple_title:         "Concatenating strings"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?

Concatenating strings is smashing two or more strings together end-to-end to make a new one. Programmers do this to construct sentences, mix dynamic data with text, or create patterns for coding elegance.

## How to:

Straight to code, here's how you do the tango with strings in Gleam:

```gleam
fn main() {
  let greeting = "Hello"
  let subject = "World"
  let exclamation = "!"

  let message = greeting ++ " " ++ subject ++ exclamation
  message
}

// Expected output: "Hello World!"
```

Piece of cake, right? Just slap strings together with `++` and you've got yourself a string stew.

## Deep Dive

Concatenation seems simple, but there's a lot under the hood. Historically, string concatenation in programming languages could get complex with different types or immutability issues. Alternatives include string formatting or building with lists, but concatenation remains a go-to for its straightforwardness.

In Gleam, which holds purity and strong typing in high regard, string concatenation uses the `++` operator that ensures the types are right and the result is a new string, every time—no side-effects here.

## See Also

For more string-based shenanigans:

- Introduction to Gleam: [https://gleam.run](https://gleam.run)
