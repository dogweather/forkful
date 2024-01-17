---
title:                "स्ट्रिंग जोड़ना"
html_title:           "Rust: स्ट्रिंग जोड़ना"
simple_title:         "स्ट्रिंग जोड़ना"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?
Concatenating strings in Rust means joining two or more strings together to create a new string. Programmers do this to combine different pieces of text or data into one string, making it easier to manipulate or print out.

## How to:
To concatenate strings in Rust, we use the `+` operator. This operator is used in between two strings to add them together. Here's an example:

```Rust
let greeting = "Hello";
let name = "Jane";
let message = greeting + " " + name;
```

In this code, we first define two strings, `greeting` and `name`. Then, we use the `+` operator to add them together and assign the result to a new string called `message`. The final value of `message` after concatenation would be `"Hello Jane"`. 

To add multiple strings, simply add more `+` operators between them. Here's another example:

```Rust
let num1 = "1";
let num2 = "2";
let num3 = "3";
let numbers = num1 + num2 + num3;
```

In this code, we are concatenating three strings containing numbers. The final value of `numbers` would be `"123"`. 

## Deep Dive:
Concatenating strings is a commonly used operation in programming, especially when dealing with text-based data. Before the `+` operator was introduced in Rust, programmers had to use the `format!()` function or the `to_string()` method to concatenate strings.

The `format!()` function uses placeholders to insert variables into a string, while the `to_string()` method converts a variable to a string, allowing it to be concatenated with other strings.

Another alternative to string concatenation is by using the `String::from()` function. This function takes in a string slice as an argument and creates a new `String` object, which can then be concatenated with other strings. 

In terms of implementation, the `+` operator is implemented through the `std::ops::Add` trait. This trait allows the `+` operator to perform addition on any type that implements it, including strings.

## See Also:
- [Rust Documentation on String Concatenation](https://doc.rust-lang.org/std/string/trait.ToString.html#method.to_string)
- [GeeksforGeeks article on string concatenation in Rust](https://www.geeksforgeeks.org/concatenate-two-strings-in-rust/)