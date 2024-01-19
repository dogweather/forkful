---
title:                "Converting a string to lower case"
html_title:           "Clojure recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Everything you need to know about Rust’s .to_lowercase()

## What & Why?
Converting a string to lower case involves replacing every uppercase character with its lowercase equivalent. We do it for things like normalization, case-insensitive comparisons, and formatting.

## How to:
Turning letters into lowercase in Rust is super simple. The `str` standard library provides us a method just for that: `.to_lowercase()`. Let's look at an easy example.

```Rust
fn main() {
    let word = "ENterPRIse";
    let low = word.to_lowercase();
    println!("{}", low);
}
```

The output would be:

```Rust
enterprise
```

That's literally it! Any input string, regardless of how mixed its casing, would uniformly become all lower case.

## Deep Dive
This operation arose out of ASCII's historical dual-case encoding, dating back more than 60 years. Many languages implement it, including Rust, so this is a reliable and effective way to go.

There are few alternatives in Rust, `.to_lowercase()` is the go-to. The method works by iterating through the string, mapping each Unicode scalar value to its lowercase counterpart.

It's important to note that this method is unicode compatible, it's not just for ASCII. For example, if you use a Turkish 'I' (U+0130), it will be properly handled:

```Rust
fn main() {
    let word = "İstanbul";
    let low = word.to_lowercase();
    println!("{}", low);
}
```

This code outputs: `i̇stanbul`, where 'ı' is the Turkish lowercase variant of 'I'. 

## See Also
Don't stop at wrapping your head around this lowercase business. Here are some related topics to explore: 

1. [Rust's standard library `str` documentation](https://doc.rust-lang.org/std/primitive.str.html)
2. [Working with strings in Rust](https://www.rust-lang.org/learn/get-started#working-with-strings)
3. Other relevant Rust methods, like `.to_uppercase()` or `.eq_ignore_ascii_case()`.