---
title:                "Capitalizing a string"
aliases: - /en/rust/capitalizing-a-string.md
date:                  2024-02-03T19:02:35.615679-07:00
model:                 gpt-4-0125-preview
simple_title:         "Capitalizing a string"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Capitalizing a string in Rust involves modifying the string so that its first character is uppercase if it's a letter, while leaving the rest of the string unchanged. Programmers often perform this operation for formatting purposes, such as preparing words for titles or ensuring consistency in user input.

## How to:

To capitalize a string in Rust, you have two primary routes: using standard library functionalities or employing third-party crates for more complex or specific needs. Here's how you can do both.

### Using Rust's Standard Library

Rust's standard library doesn't provide a direct method to capitalize strings, but you can achieve this by manipulating the string's characters.

```rust
fn capitalize_first(s: &str) -> String {
    let mut c = s.chars();
    match c.next() {
        None => String::new(),
        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
    }
}

fn main() {
    let my_string = "hello";
    println!("{}", capitalize_first(my_string)); // Output: Hello
}
```

### Using the `heck` Crate

For a more straightforward approach, especially when working within a larger text processing context, you might prefer using third-party libraries such as `heck`. The `heck` crate offers various case conversion functionalities, including a simple way to capitalize strings.

First, add `heck` to your `Cargo.toml`:

```toml
[dependencies]
heck = "0.4.0"
```

Then, use it to capitalize your string:

```rust
extern crate heck; // Not needed in Rust 2018 edition or later
use heck::TitleCase;

fn main() {
    let my_string = "hello world";
    let capitalized = my_string.to_title_case();
    println!("{}", capitalized); // Output: Hello World
}
```

Note: The `to_title_case` method provided by `heck` capitalizes each word in the string, which might be more than what you're looking for if you only want the first character of the string capitalized. Adjust your usage according to your specific needs.
