---
title:                "Interpolating a string"
html_title:           "Arduino recipe: Interpolating a string"
simple_title:         "Interpolating a string"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

String interpolation is the process of embedding variables into a string. This is typically performed to create more dynamic and flexible strings.

## How to:

String interpolation in Rust is achieved through the `format!`macro. Let's take a look at how it's done:

```Rust
let name = "John";
let age = 30;
let result = format!("{} is {} years old.", name, age);
println!("{}", result);
```

When you run this, the output would be:
```
John is 30 years old.
```

## Deep Dive

String interpolation originated from the scripting languages back in the '80s, primarily to simplify the process of combining strings and variables. Rust, however, favors explicit type formatting over implicit (like `"${variable}"` in some languages).

As an alternative to `format!`, Rust also offers the `println!`macro for output (e.g., `println!("{} is {} years old.", name, age);`).

The key point in Rust's implementation is the type safety - variables must be explicitly converted to strings. This avoids errors caused by unexpected types.

## See Also

For further reading, here are a couple helpful links:

1. [Rust's `format!` macro documentation](https://doc.rust-lang.org/std/macro.format.html)

2. [String Interpolation in Rust](https://www.programming-idioms.org/idiom/70/interpolate-variables-in-string/1047/rust)