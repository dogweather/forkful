---
title:                "Rust recipe: Searching and replacing text"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why

Editing and manipulating text is a core task in any programming language, and Rust is no exception. Whether you're working on a small project or a large-scale application, having the ability to efficiently search and replace text can save you time and effort. In this blog post, we'll take a look at how to perform this task in Rust and explore some of its features that make it a powerful language for text manipulation.

## How To

To start off, let's take a look at a simple example of searching and replacing text in Rust. We'll use the `replace` method from the `String` type, which allows us to search for a specific substring and replace it with another. Here's the code:

```rust
fn main() {
    let mut text = String::from("Rust programming is fun and challenging.");
    text.replace("fun", "exciting");
    println!("{}", text);
}
```

In this example, we have a string that contains the phrase "Rust programming is fun and challenging." We use the `replace` method to search for the word "fun" and replace it with "exciting". Running this code will produce the output: "Rust programming is exciting and challenging."

Apart from simple substitutions, Rust also offers more advanced methods for searching and replacing text. These include pattern-matching using regular expressions, case-insensitive matching, and global substitutions. Here's an example using regular expressions:

```rust
fn main() {
    let mut text = String::from("My email is example@example.com");
    text = regex::Regex::new(r"^([a-z0-9]+)@([a-z0-9]+)\.([a-z]{2,})$")
        .unwrap()
        .replace_all(&text, "$1 (at) $2 (dot) $3");
    println!("{}", text);
}
```

This code uses the `regex` crate to search for and replace an email address with a more human-friendly format. The output would be "My email is example (at) example (dot) com."

## Deep Dive

One of Rust's unique features is its ownership and borrowing system, which allows for efficient text manipulation without any memory leaks or dangling pointers. This means that even when we perform operations such as searching and replacing on a string, we don't have to worry about freeing up memory afterward.

Additionally, Rust's `String` type implements the `FromStr` trait, which allows for easy and efficient parsing of text. This is especially useful when working with large files or streams.

Another advantage of using Rust for text manipulation is its performance. Rust is a systems programming language, which means that it's designed for speed and efficiency. This makes it an ideal choice for any tasks that require high-performance text manipulation.

## See Also

- [Rust String Documentation](https://doc.rust-lang.org/std/string/struct.String.html)
- [The Power of Regular Expressions in Rust](https://medium.com/swlh/the-power-of-regular-expressions-in-rust-94f9b5c2d193)
- [An Introduction to Rust's Ownership and Borrowing](https://stevedonovan.github.io/rust-gentle-intro/3-ownership.html)

In conclusion, Rust offers a range of powerful features for searching and replacing text, making it an excellent choice for any programming task that involves text manipulation. Using its ownership system and efficient parsing abilities, we can perform these operations without worrying about memory leaks or performance issues. Give it a try in your next project and see how it works for you!