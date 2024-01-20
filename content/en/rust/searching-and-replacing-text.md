---
title:                "Searching and replacing text"
html_title:           "Arduino recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Finessing Text Manipulation in Rust

## What & Why?
All coders, at some point, need to twiddle with strings: sift through text and swap parts of it. This is search-and-replace. It is a way to automate repetitive tasks, correct errors, and format data properly.

## How to:

In Rust, you can easily perform search and replace operations with the `replace()` function from the String class.

Let's go through a simple example code snippet and see the output:

```Rust
fn main() {
    let my_string = String::from("Hello, World!");
    let new_string = my_string.replace("World", "Rust");
    println!("{}", new_string);
}
```

Output:
```Shell
Hello, Rust!
```

The phrase "Hello, World!" got turned into "Hello, Rust!". It's as plain and straightforward as that.

## Deep Dive

Rust's `replace()` method is part of its standard library. Derived from the heritage of C-style string manipulation, Rust modernizes and simplifies this primal operation, making it idiomatic and safer to use.

Alternative ways to perform search and replace in Rust include using the `str::replace()` method or even regex via the `regex` crate, especially if you have complex search patterns.

Under the hood, `replace()` in Rust uses a fairly futureproof and efficient algorithm. It scans the input string from start to end, checks for instances of the 'search' text, and upon finding a match, it seamlessly substitutes it with the 'replace' text.

## See Also

For more on string manipulation in Rust, check out these sources:
- [Rust API Documentation](https://doc.rust-lang.org/std/string/struct.String.html)
- Regex usage in Rust: [rust-lang/regex](https://github.com/rust-lang/regex)
- More on Rust: [The Rust Programming Language](https://doc.rust-lang.org/book/)