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

## Why

It's a common programming task to find the length of a string. Whether you're working on a simple text processing program or a more complex project, knowing how to find the length of a string can come in handy.

## How To

```rust
let my_string = "Hello World!";
let length = my_string.len();

println!("The length of my string is {}", length);
```

Output: The length of my string is 12.

In Rust, strings are UTF-8 encoded, which means that each character is represented by a varying number of bytes. Therefore, to find the length of a string, we use the `len()` method which returns the number of bytes in the string.

Another way to find the length of a string is to use the `chars()` method, which returns an iterator over the characters of the string. We can then use the `count()` method to count the number of characters in the string.

```rust
let my_string = "Rust is awesome!";
let char_count = my_string.chars().count();

println!("The string has {} characters", char_count);
```

Output: The string has 16 characters.

## Deep Dive

When working with strings in Rust, it's important to keep in mind that strings are immutable by default. This means that any operations on a string will return a new string, rather than modifying the original one. This is why the `len()` method does not have any parameters - it is simply returning the length of the original string.

Additionally, the `len()` method only counts the number of bytes in a string, not the number of visible characters. This is because some characters may be represented by multiple bytes, depending on the encoding used. If you want the length of a string in terms of visible characters, using the `chars()` method is a more accurate approach.

## See Also

For more information on strings and the methods mentioned above, check out the official Rust documentation:

- [The Rust Programming Language](https://doc.rust-lang.org/book/ch08-02-strings.html)
- [Rust Standard Library - Strings](https://doc.rust-lang.org/std/string/index.html)
- [Rust By Example - Strings](https://doc.rust-lang.org/stable/rust-by-example/std/str.html)