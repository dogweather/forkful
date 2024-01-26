---
title:                "Printing debug output"
date:                  2024-01-20T17:52:27.522287-07:00
model:                 gpt-4-1106-preview
simple_title:         "Printing debug output"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?
Printing debug output lets you spit out values to check if your program behaves as expected. It's your bread and butter for quick-and-dirty troubleshooting when things go awry.

## How to:
```gleam
import gleam/io

pub fn main() {
  let my_variable = "Debugging in Gleam is straightforward!";
  io.debug(my_variable)
}
```
Run it, and you'll see `Debugging in Gleam is straightforward!` in your terminal. It shows you what's up with your code at that moment.

## Deep Dive
Historically, printing debug output harks back to days when logging tools were luxury and rubber ducks weren’t on desks. It's the first tool a dev thinks of even now, despite advanced debugging tools.

In Gleam, `io.debug` is the go-to. Alternatives include more structured logging libraries when you outgrow simple print statements. Under the hood, `io.debug` writes to standard error, making it distinguishable from standard output.

## See Also
- The Gleam `io` module documentation: https://hexdocs.pm/gleam_stdlib/gleam/io/
- Structured logging in Gleam: [Module Placeholder, based on version updates]
- A guide on debugging in functional programming: [Link Placeholder, subject to available resources]
