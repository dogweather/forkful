---
title:    "Rust recipe: Concatenating strings"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Why

Strings are an important part of any programming language, as they allow us to manipulate and display text. In Rust, strings are represented as collections of characters, and one common task when working with strings is concatenating them. In this blog post, we will explore why and how to concatenate strings in Rust.

## How To

Concatenating strings in Rust is a relatively simple task. We can use the `+` operator or the `format!` macro to join multiple strings together. Let's look at an example using the `+` operator:

```
let greeting = "Hello";
let name = "John";
let sentence = greeting + " " + name;
println!("{}", sentence);
```

The output of this code would be `Hello John`. As you can see, we simply used the `+` operator to concatenate the two strings together and create a new string.

We can also use the `format!` macro to achieve the same result, but with a slightly different syntax:

```
let greeting = "Hello";
let name = "John";
let sentence = format!("{} {}", greeting, name);
println!("{}", sentence);
```

The output would be the same as before. The `format!` macro works by taking in a format string (the first argument) and replacing the format specifiers (such as `{}`) with the values provided after the format string.

It's worth noting that both the `+` operator and the `format!` macro take ownership of the strings they are concatenating. This means that the original strings are no longer valid after the concatenation. To avoid this, we can use the `&` operator to create a reference to the strings and avoid transferring ownership.

## Deep Dive

In Rust, strings are implemented as UTF-8 encoded byte sequences, which means they can contain characters from all languages and scripts. This also means that when concatenating strings, we need to be mindful of the encoding to avoid any issues.

One important thing to keep in mind when concatenating strings using the `+` operator is that it creates a new string every time, which can be inefficient for large strings. To address this, Rust provides the `std::string::String` type, which allows us to modify strings in place without creating new ones. This can be useful when dealing with large amounts of text.

## See Also

- [Official Rust documentation on Strings](https://doc.rust-lang.org/std/string/index.html)
- [Rust Book: Common Operations on Strings](https://doc.rust-lang.org/book/ch08-02-strings.html#common-operations-on-strings)
- [Concatenation in Rust vs C++](https://medium.com/coding-in-simple-english/string-concatenation-in-rust-vs-c-a153233e5c95)

---

_Thank you for reading! Stay tuned for more Rust programming blog posts._