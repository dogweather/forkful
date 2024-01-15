---
title:                "Capitalizing a string"
html_title:           "Rust recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why

Capitalizing strings may seem like a simple task, but it can be crucial in certain programming situations. For example, when working with user inputs or displaying data in a consistent format, capitalizing strings can improve readability and organization.

## How To

First, we need to import the `String` type from the standard library with the `use` keyword:

```rust
use std::string::String;
```

Next, we can use the `to_uppercase` method on the `String` type to capitalize a given string:

```rust
let my_string = String::from("hello world");
let capitalized_string = my_string.to_uppercase();

println!("{}", capitalized_string); // Output: "HELLO WORLD"
```

We can also use the `to_uppercase` method on a `str` type, which is the same as a string slice, without needing to create a new `String` instance:

```rust
let my_string = "hello world";
let capitalized_string = my_string.to_uppercase();

println!("{}", capitalized_string); // Output: "HELLO WORLD"
```

## Deep Dive

Behind the scenes, the `to_uppercase` method uses the `Unicode` standard for capitalizing strings. This means that any non-English characters, such as accented letters, will be properly capitalized as well.

In addition, the `to_uppercase` method is also language-aware, meaning it takes into account various rules and conventions for capitalization in different languages. For example, in German, the letter "ß" is replaced with "SS" when capitalized, and the `to_uppercase` method handles this automatically.

Furthermore, the `to_uppercase` method is not just limited to letters, it also handles capitalizing numbers and symbols, as they may have different capitalization rules in different languages.

## See Also

For more information on the `to_uppercase` method and other string manipulation techniques in Rust, check out the official Rust documentation and other online resources:
- [Rust Official Documentation](https://doc.rust-lang.org/std/string/struct.String.html#method.to_uppercase)
- [Rust By Example: Strings](https://doc.rust-lang.org/rust-by-example/std/str.html)
- [Rust Cookbook: Strings](https://rust-lang-nursery.github.io/rust-cookbook/strings.html)
- [Rust Language Cheat Sheet: Strings](https://cheats.rs/#strings)