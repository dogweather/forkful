---
title:                "Printing debug output"
html_title:           "Gleam recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?

Printing debug output, simply put, lets you see the values of variables and other data during the execution of your program. It's a programmer's best mate in busting bugs because it aids in figuring out where things may be going pear-shaped. 

## How To:

Getting debug output in Gleam is a cinch. Here's a basic example:

```Gleam
import gleam/io

fn main() {
  let x = 1
  io.debug(x) // Prints: 1
}
```
This will print the value of `x`, in this case `1`, to your terminal's standard error stream (`stderr`). 

## Deep Dive

The practice of tapping into standard IO for debugging has a time-honoured history, starting with good ol' `print` statements in many early programming languages. 

Gleam inherits this tradition, with a functional programming spin, via `io.debug/1` function. This function, like in other Beam languages (e.g., Erlang, Elixir), writes to `stderr` rather than `stdout`. This distinction is data vital as `stderr` doesn’t get redirected, thus allowing you to see your debug printouts even when your program's output is piped or redirected to a file. 

You might be tempted to go all willy-nilly with debug output, but there’s a cleaner way: good logging practices. Instead of leaving debug output in production code, use logging libraries that can be configured to print debug information only when needed. 

## See Also

For more reads on debug output and logging in Gleam, take a gander at these gubbins:

- Gleam's IO Documentation: <https://hexdocs.pm/gleam_stdlib/gleam/io.html> 
- Logging in Beam Languages: <https://medium.com/beam-language/logging-in-beam-languages-2c191ee60ebf>
- How to Debug Gleam Programs: <https://github.com/gleam-lang/gleam/discussions/394>