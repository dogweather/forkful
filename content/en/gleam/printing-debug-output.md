---
title:                "Printing debug output"
html_title:           "Arduino recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

# Printing Debug Output in Gleam

## What & Why?

Debug output is a convenient way to inspect data within your Gleam program, serving like a window into your code's inner workings. Programmers use it to check data at a specific point in code, helping identify and debug issues.

## How To:

In Gleam, we use the built-in `debug` function to print debug output.

```Gleam
import gleam/io

fn main() {
  let mystery_number = 42
  io.debug(mystery_number)
}
```
Executing the above will output:
```Bash
42
```

## Deep Dive

Debugging, a practice as old as programming itself, is the process of identifying and fixing errors in the code. Built-in debug functions, like `io.debug` in Gleam, are a leap from primitive methods of manual code tracing. 

An alternative to `io.debug` in Gleam is `io.display` which, unlike `debug`, evaluates the string version of an expression and also neatly formats the display of complex datatypes.

```Gleam
import gleam/io

fn main() {
  let complex_data = tuple("Hello", 42, Nil)
  io.display(complex_data)
}
```
Gives you:
```Bash
#("Hello", 42, [])
```
The implementation of these functions in the Gleam core library revolves around evaluating code at runtime, converting values to strings, and printing the strings to stdout.

## See Also

- Great overview of debugging: [https://en.wikipedia.org/wiki/Debugging](https://en.wikipedia.org/wiki/Debugging)
- XML for Erlang (to use with Gleam): [https://github.com/willemdj/erlsom](https://github.com/willemdj/erlsom)