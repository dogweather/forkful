---
title:                "Converting a string to lower case"
html_title:           "Rust recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?

Converting a string to lower case in Rust is the process of changing all of the alphabetic characters in a string to their equivalent lower case letters. This is a common task for programmers, especially when dealing with user input or comparing strings in a case-insensitive manner.

## How to:

Converting a string to lower case in Rust is simple and can be done using the `to_lowercase()` method. Here's an example:

```Rust
let input = "HELLO WORLD";
let lowercase = input.to_lowercase();

println!("{}", lowercase); // hello world
```

If you don't want to create a new string, you can also use the `make_ascii_lowercase()` method to modify the original string in-place. Here's an example:

```Rust
let mut input = String::from("HELLO WORLD");
input.make_ascii_lowercase();

println!("{}", input); // hello world
```

## Deep Dive

In the early days of programming, character sets were limited and did not include both upper and lower case letters. This meant that strings were often converted to upper case for easier manipulation. However, as character sets evolved and began to include both upper and lower case letters, programmers needed a way to convert strings to lower case.

In Rust, there are two ways to convert a string to lower case - `to_lowercase()` and `make_ascii_lowercase()`. The former method creates a new string with the converted characters, while the latter modifies the original string in-place. Which one you use will depend on your specific use case.

As an alternative to these Rust-specific methods, you can also use the `ToLowercase` trait from the standard library's `unicode` module. This trait allows you to convert strings to lower case using different languages or locales. It can also be used to create iterators over the converted characters, which can be useful for more complex manipulations.

## See Also

- [Rust String Documentation](https://doc.rust-lang.org/std/string/)
- [Standard Library `unicode` Module Documentation](https://doc.rust-lang.org/std/unicode/index.html)
- [Rust `std::char::ToLowercase` Documentation](https://doc.rust-lang.org/std/char/trait.ToLowercase.html)