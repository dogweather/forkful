---
title:                "Concatenating strings"
html_title:           "Rust recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?

Concatenating strings in Rust refers to the process of combining multiple strings into one single string. Programmers do this for various reasons, such as creating longer strings to be displayed or stored, or to manipulate the data within the strings.

## How to:

Concatenating strings in Rust is a simple process that can be done using the `+` operator or the `format!` macro. Here are two examples showing how to concatenate strings:

```
// Using the `+` operator:
let string1 = "Hello";
let string2 = "World";
let concatenated_string = string1 + " " + string2;
println!("{}", concatenated_string); // output: Hello World
```

```
// Using the `format!` macro:
let string1 = "Programming";
let string2 = "is";
let string3 = "fun!";
let concatenated_string = format!("{} {} {}", string1, string2, string3);
println!("{}", concatenated_string); // output: Programming is fun!
```

## Deep Dive:

Concatenating strings has been a common practice in programming languages for manipulating textual data. In Rust, there are two main alternatives to concatenating strings - `push_str` and `push` methods. These methods modify the first string directly instead of creating a new string, making them more efficient for longer strings.

In terms of implementation, the `+` operator and `format!` macro both use the `Add` trait, allowing them to perform string concatenation. Additionally, `format!` also uses the `ToString` trait to convert non-string data types into strings before concatenation.

## See Also:

Check out the official documentation for more information on string concatenation in Rust: https://doc.rust-lang.org/book/ch08-02-strings.html#concatenation.