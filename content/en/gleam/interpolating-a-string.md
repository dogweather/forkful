---
title:                "Interpolating a string"
date:                  2024-01-20T17:50:38.766516-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolating a string"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

String interpolation is the process of embedding expressions into string literals to create new string values. Programmers interpolate strings to dynamically construct messages, often for user output or logging.

## How to:

In Gleam, string interpolation is straightforward. Use the `#{}` syntax to insert expressions into strings. Here's a quick example:

```gleam
fn main() {
  let name = "world"
  let greeting = "Hello, #{name}!"
  greeting
}

// This will output: "Hello, world!"
```

## Deep Dive

Historically, string concatenation was the norm, where you'd manually join strings and values. It gets messy fast. Interpolation is cleaner and more readable.

Languages vary in their interpolation syntax; Gleam's `#{}` mirrors Ruby's and Elixir's. This consistency is helpful for folks hopping between languages.

Under the hood, Gleam's compiler transforms interpolated strings into a series of string concatenations before it's compiled to Erlang, the language Gleam compiles into. So:

```gleam
"Hello, #{name}!"
```

becomes something like (in Erlang pseudo-code):

```erlang
"Hello, " ++ name ++ "!"
```

The choice of interpolation over concatenation is usually about readability and convenience, though there's not much performance difference due to compiler optimizations.

## See Also

- [The Gleam Book](https://gleam.run/book/)
- [Erlang's String module documentation](http://erlang.org/doc/man/string.html) for background on what Gleam compiles down to.
