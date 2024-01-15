---
title:                "串接字符串"
html_title:           "Rust: 串接字符串"
simple_title:         "串接字符串"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why

Concatenating strings is a common operation in programming that involves combining multiple strings into one larger string. It can be useful for formatting strings, building dynamic messages, or manipulating data.

## How To

To concatenate strings in Rust, you can use the `+` operator or the `format!` macro. Let's look at some examples:

````Rust
// Using the + operator
let first_name = "John";
let last_name = "Doe";
let full_name = first_name + " " + last_name;
println!("{}", full_name); // Output: John Doe

// Using the format! macro
let age = 30;
let greeting = format!("Hello, my name is {} and I am {} years old.", full_name, age);
println!("{}", greeting); // Output: Hello, my name is John Doe and I am 30 years old.
````

Both methods involve combining multiple strings, but the `+` operator requires all operands to be of type `String`, while the `format!` macro can handle different types such as integers or floats.

## Deep Dive

In Rust, strings are represented as byte arrays, which means they are not mutable by default. This means that when we "concatenate" strings using the `+` operator or the `format!` macro, we are actually creating new strings and copying the data from the original strings into the new one.

To avoid this extra copying, Rust has the `String::push_str` method, which appends the contents of one string to another without creating a new string. This is more efficient when working with larger strings. Let's see an example:

````Rust
let mut message = String::from("Hello");
let name = "Jane";
message.push_str(", ");
message.push_str(name);
println!("{}", message); // Output: Hello, Jane
````

It's important to note that the `push_str` method only works with `String` types, so if you have a `&str` (string slice), you will need to convert it to a `String` before using this method.

## See Also

- [Rust by Example - Strings](https://doc.rust-lang.org/stable/rust-by-example/std/str.html)
- [Rust String Documentation](https://doc.rust-lang.org/std/string/struct.String.html)
- [Rust Programming Language Book - Strings](https://doc.rust-lang.org/book/ch08-02-strings.html)