---
title:                "Interpolating a string"
html_title:           "Gleam recipe: Interpolating a string"
simple_title:         "Interpolating a string"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Interpolating a string is a method of injecting variables into a string. It reduces complexity and increases readability by providing an alternative to unwieldy concatenation.

## How to:

Here's an easy interpolation example in Gleam:

```gleam
let name = "Gleam"
let output = "Hello, #(name) programmer!"

// Outputs: "Hello, Gleam programmer!"
```

It's a matter of using `#()` to contain variables you want to interpolate in your text.

## Deep Dive

Interpolation was initially popularized by Perl before becoming staples in languages like Ruby and JavaScript. Before interpolation, the common approach was string concatenation, splitting a string into separate pieces that are constructed with variables and appended together, like so:

```gleam
let name = "Gleam"
let output = "Hello, " ++ name ++ " programmer!"
```

This can get out of hand pretty quickly for longer, more complex strings. Thus, we use string interpolation as a more effective alternative.

Now, the `#()` used in Gleam's string interpolation isn't magic. Under the hood, the compiler translates them into function calls. Which means `#(name)` is equivalent to: 

```gleam
let name = "Gleam"
let output = "Hello, " ++ name.toString() ++ " programmer!"
```

Gleam simply makes it easier to use these calls in the context of string manipulation.

## See Also:

For further reading:

1. The Gleam code style guide - [https://gleam.run/book/tour/](https://gleam.run/book/tour/)
2. Detailed overview of Gleam's string manipulation approaches - [https://gleam.run/book/tour/string-manipulation.html](https://gleam.run/book/tour/string-manipulation.html)