---
title:                "Finding the length of a string"
html_title:           "Rust recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Finding the length of a string is a common task in programming, where we calculate the number of characters in a string. This is useful for various operations such as manipulating strings, validating input, and formatting output. It allows us to work with strings of different lengths and ensures we can accurately process data.

## How to:

Finding the length of a string in Rust is simple and can be done using the built-in `len()` method. The `len()` method can be applied to any string, including literal strings and string variables.

```Rust
let string = "Hello, world!";
let length = string.len();

println!("The length of the string is: {}", length);

// Output: The length of the string is: 13
```

You can also find the length of a string using a `for` loop, which iterates through each character in the string and increments a counter variable.

```Rust
let string = "Hello, world!";
let mut length = 0;

for character in string.chars() {
    length += 1;
}

println!("The length of the string is: {}", length);

// Output: The length of the string is: 13
```

## Deep Dive:

In Rust, strings are a collection of characters stored as `String` values. Strings can be represented in various ways, including UTF-8 byte sequences, UTF-16 code units, and more. Therefore, when finding the length of a string, Rust takes into account the different representations to accurately calculate the number of characters.

An alternative to using the `len()` method or a `for` loop is to use the `String::chars()` method, which returns an iterator of characters in the string. This iterator can then be used to calculate the length of the string.

There are also libraries in Rust, such as the `unicode-segmentation` crate, which provide more advanced functions for finding the length of strings, taking into account language-specific rules and characters.

## See Also:

- [Rust Standard Library](https://doc.rust-lang.org/nightly/std/string/struct.String.html)
- [The Rust Book](https://doc.rust-lang.org/book/ch08-02-strings.html)
- [unicode-segmentation crate](https://crates.io/crates/unicode-segmentation)