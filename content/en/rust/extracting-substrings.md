---
title:                "Rust recipe: Extracting substrings"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why

When writing code in Rust, at some point you may need to manipulate strings and extract substrings from them. This could be for tasks such as parsing user input or formatting data. Luckily, Rust has built-in methods for extracting substrings that make this process efficient and easy. In this blog post, we will explore how to extract substrings in Rust.

## How To

To extract a substring in Rust, we use the `get()` method on a `String` or `&str` variable. This method takes two parameters: the starting index and the ending index of the substring we want to extract. Let's look at an example:

```rust
let word = "Hello World";
let sub = word.get(0..5);
```

In this code, we have a string variable `word` containing the phrase "Hello World." We then use the `get()` method to extract the substring from the first index (0) to the fifth index (not inclusive). The result will be a new `&str` variable `sub` containing the substring "Hello."

If we want to get the whole word "World," we can use the `get()` method with a range of indices:

```rust
let word = "Hello World";
let sub = word.get(6..);
```

In this case, we omit the second index, which means we want to extract all the characters from the sixth index until the end of the string. The result will be a new `&str` variable `sub` containing the substring "World."

## Deep Dive

Under the hood, the `get()` method uses the slice syntax in Rust. This syntax allows us to take a portion of a string or an array without creating a new memory allocation. This means that the substring we extracted using the `get()` method will still refer to the original string, saving memory and making our code more efficient.

It's also worth noting that the first index in Rust is inclusive, while the second index is not. So, when using `get()`, we need to specify the index where we want the substring to start and the index right after where we want it to end.

Additionally, we can also use the `&str` slice type directly without the `get()` method. For example:

```rust
let word = "Hello World";
let sub = &word[0..5];
```

This does the same thing as using `get()` and returns a `&str` variable containing the substring "Hello." However, using the `get()` method is preferred as it performs bounds checking and ensures we do not try to access indices that do not exist.

## See Also

For more information on extracting substrings in Rust, check out the official documentation:

- [The Rust Book: Strings](https://doc.rust-lang.org/book/ch08-02-strings.html#slices)
- [Rust By Example: Strings and Slices](https://doc.rust-lang.org/rust-by-example/std/str.html#slices)
- [Rust String Slice Cheatsheet](https://gist.github.com/joearasin/baff5fc60497e0d5a4c5796c56977aac)