---
title:                "Capitalizing a string"
date:                  2024-01-19
html_title:           "C recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Capitalizing a string means making the first letter of each word uppercase while the rest remain lowercase. Programmers do this for formatting purposes, to adhere to linguistic norms in user interfaces, or to ensure data consistency in text processing.

## How to:

Rust does not include a built-in method to capitalize each word in a string, but we can easily implement our own using the `to_ascii_uppercase` method for single characters and looping through the words.

```Rust
fn capitalize_words(s: &str) -> String {
    s.split_whitespace()
        .map(|word| {
            let mut chars = word.chars();
            match chars.next() {
                None => String::new(),
                Some(first) => first.to_ascii_uppercase().to_string() + chars.as_str(),
            }
        })
        .collect::<Vec<_>>()
        .join(" ")
}

fn main() {
    let sentence = "hello world";
    println!("{}", capitalize_words(sentence));
}
```

Sample output:

```
Hello World
```

## Deep Dive:

Historically, Rust has prioritized a minimal standard library, with many utility functions provided by the community through crates. For string capitalization, you can use the `heck` crate for more advanced case conversions, such as CamelCase, snake_case, and more.

Capitalizing a string can be tricky with unicode characters. Rust's `char` type is a Unicode scalar value, allowing for proper handling of most characters. When dealing with full Unicode normalization, more advanced libraries, such as `unicode-segmentation`, should be considered for operations that are mindful of grapheme clusters.

Implementation-wise, our `capitalize_words` function is not highly performant as it allocates a new `String` for each word. In applications that require high performance, it would be beneficial to optimize string manipulation to avoid excessive memory allocations.

## See Also:

- Rust documentation for 'char': https://doc.rust-lang.org/std/primitive.char.html
- 'Heck' crate for case conversions: https://crates.io/crates/heck
- 'Unicode Normalization Forms' in Rust: https://unicode-rs.github.io/unicode-normalization/unicode_normalization/index.html
- Rust Book for more on strings: https://doc.rust-lang.org/book/ch08-02-strings.html
