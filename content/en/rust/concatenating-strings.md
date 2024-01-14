---
title:                "Rust recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why

Concatenating strings is a common task in programming, where we combine two or more strings to form a new one. In Rust, it is important to understand the different ways to concatenate strings to improve code efficiency and readability.

## How To

```rust
// Using the `+` operator
let greeting = "Hello";
let name = "John";
let message = greeting + " " + name;
println!("{}", message); // This will print "Hello John"

// Using the `format!()` macro
let age = 25;
let info = format!("{} is {} years old", name, age);
println!("{}", info); // This will print "John is 25 years old"

// Using the `push_str()` method
let mut sentence = String::from("I love");
sentence.push_str(" Rust");
println!("{}", sentence); // This will print "I love Rust"

// Using the `insert()` method
let mut phrase = String::from("World!");
phrase.insert(0, "Hello ");
println!("{}", phrase); // This will print "Hello World!"
```

There are multiple ways to concatenate strings in Rust, but the most common and efficient ones are using the `+` operator and the `format!()` macro. The `+` operator allows us to easily combine two strings, while the `format!()` macro is useful for more complex concatenation with different data types. The `push_str()` method is used to add a string at the end of an existing one, while the `insert()` method is used to insert a string at a specific index.

## Deep Dive

One important thing to note when concatenating strings in Rust is that it involves memory allocation. The `String` type in Rust is stored as a vector of bytes, with the added functionality of managing the memory allocation. When we use the `+` operator or the `push_str()` method, the content of the original string is copied to a new memory location with the added string. This can lead to performance issues, especially when concatenating multiple strings in a loop. To avoid this, we can use the `format!()` macro or the `insert()` method, which both create a new string without copying the original data.

Additionally, Rust has the `join()` method, which allows us to concatenate multiple strings in a more efficient way by avoiding extra memory allocations and copies.

## See Also

- [Rust String Documentation](https://doc.rust-lang.org/std/string/struct.String.html)
- [Efficient String Concatenation in Rust](https://medium.com/snowplow-analytics/efficient-string-concatenation-in-rust-210fcbee9538)
- [Rust By Example: Strings](https://doc.rust-lang.org/stable/rust-by-example/std/str.html)