---
title:                "Finding the length of a string"
html_title:           "Arduino recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Finding the length of a string in programming is getting the count of characters within a string. Programmers do it when they need to manipulate, validate, or analyze texts efficiently and accurately.

## How to:

```Rust
fn main() {
    let my_str = "Hello, Rust!";
    println!("Length: {}", my_str.len());
}
```
Output:
```
Length: 12
```
The method `len()` is native to the data type String in Rust. It allows you to get the number of bytes in a string quickly.

## Deep Dive

Rust's `len()` function, introduced from its first stable release in 2015, returns the length in bytes, not the count of Unicode grapheme clusters. In other words, it might not always exactly match what we visually perceive as characters, especially with multi-byte characters.

Alternatives methods to `len()` exist, like the `.chars().count()` function. This method counts Unicode Scalar Values (i.e., a single Unicode character, excluding "surrogate" characters).

```Rust
fn main() {
    let my_str = "こんにちは";
    println!("Length: {}", my_str.chars().count());
}
```
Output:
```
Length: 5
```
When dealing with non-Latin string inputs, using 'chars().count()' can be more practical.

The length returned by `len()` is in O(1) time complexity operation because Rust's string is implemented as a Vector of Bytes, which inherently knows its length. Conversely, `.chars().count()`, being an iterator, has to traverse all characters, making it an O(N) operation.

## See Also

- Rust's String documentation: https://doc.rust-lang.org/std/string/struct.String.html.
- Rust's `len()` usage: https://doc.rust-lang.org/std/string/struct.String.html#method.len.
- Unicode Scalar Values in Rust: https://doc.rust-lang.org/std/char/index.html.