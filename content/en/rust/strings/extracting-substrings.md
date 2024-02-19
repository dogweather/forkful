---
aliases:
- /en/rust/extracting-substrings/
date: 2024-01-20 17:46:21.805021-07:00
description: "Extracting substrings means grabbing a smaller piece from a larger string\u2014\
  kind of like slicing a cake to get a piece you actually want to eat. Programmers\u2026"
lastmod: 2024-02-18 23:09:10.837846
model: gpt-4-1106-preview
summary: "Extracting substrings means grabbing a smaller piece from a larger string\u2014\
  kind of like slicing a cake to get a piece you actually want to eat. Programmers\u2026"
title: Extracting substrings
---

{{< edit_this_page >}}

## What & Why?

Extracting substrings means grabbing a smaller piece from a larger string—kind of like slicing a cake to get a piece you actually want to eat. Programmers do this to pluck out data, parse text, or simply break down information for easier handling.

## How to:

Let's get our hands dirty with Rust. Imagine you've got a string, and you want a specific part of it. You can use slicing `&str[start..end]` where `start` is where you begin, and `end` is where you stop.

```Rust
fn main() {
    let text = "The quick brown fox jumps over the lazy dog";
    let quick_brown = &text[4..15]; // Slices from 4th to 14th index
    println!("{}", quick_brown); // Outputs: quick brown
}
```

Slicing is neat, but it can lead to panics if your indices don't fall on character boundaries. To prevent this, Rust provides methods like `get`:

```Rust
fn main() {
    let text = "The quick brown fox";
    match text.get(4..15) {
        Some(substring) => println!("{}", substring), // safe slicing
        None => println!("Slice is out of bounds."),
    }
}

// Outputs: quick brown
```

There you have it—a quick peek at substring extraction in Rust. Look how easy that was!

## Deep Dive

Slicing in languages with UTF-8 encoded strings like Rust is a bit tricky—characters can be more than one byte! Before Rust, in languages like C, string handling could be a bug-ridden headache, as you manually managed memory.

Rust's `str` type is a sequence of UTF-8 bytes, always valid UTF-8. Extracting substrings safely respects these character boundaries.

Alternatives to slicing include using iterators or regex for more complex patterns, but they come with overhead. When slicing, Rust checks byte indices align with char boundaries at runtime, preventing potential crashes from invalid slices.

## See Also

- Rust Book on strings: https://doc.rust-lang.org/book/ch08-02-strings.html
- Rust by Example – Strings: https://doc.rust-lang.org/rust-by-example/std/str.html
- Rust docs for `str`: https://doc.rust-lang.org/std/primitive.str.html
