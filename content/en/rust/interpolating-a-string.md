---
title:                "Interpolating a string"
html_title:           "Rust recipe: Interpolating a string"
simple_title:         "Interpolating a string"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Interpolating a string in Rust means inserting values into a string dynamically, often used for formatting text for display or output. Programmers use this to make their code more concise and readable by avoiding concatenation of multiple strings.

## How to:
In Rust, you can interpolate a string using the `format!` macro. It takes in a format string, followed by the values to be inserted into the string.

```Rust
let name = "John";
let age = 25;
println!("Hello, my name is {} and I am {} years old.", name, age);
// Output: Hello, my name is John and I am 25 years old.
```

The values can be inserted in any order and also be type annotated to ensure correct formatting.

```Rust
let name = "Emily";
let age = 30;
println!("My name is {name} and my age is {age}.", age = age, name = name);
// Output: My name is Emily and my age is 30.
```

Interpolating multiple values in the same string is also possible by using braces and the corresponding value names.

```Rust
let name = "Alex";
let age = 20;
let city = "New York";
println!("Hey, {name}, I'm {age} and I live in {city}.", name = name, age = age, city = city);
// Output: Hey, Alex, I'm 20 and I live in New York.
```

## Deep Dive:
The `format!` macro was added to Rust in version 1.2, replacing the `write!` macro for string formatting. It uses the `Display` trait, a Rust feature that allows types to be converted to strings for printing. This provides a more concise and readable way of formatting strings compared to traditional concatenation methods.

An alternative to string interpolation is using the `println!` macro with a placeholder for the values to be inserted. However, this only prints the output and does not return a string.

## See Also:
- [The Rust Book](https://doc.rust-lang.org/book/ch08-03-hash-maps.html#pretty-printing-with-format)
- [Rust Standard Library](https://doc.rust-lang.org/std/fmt/#formatting-traits)