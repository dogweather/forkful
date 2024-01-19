---
title:                "Extracting substrings"
html_title:           "Arduino recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

# Extracting Substrings in Rust: A Concise Look
## What & Why?

Extracting substrings is taking a smaller string from a larger one. Programmers do it to facilitate data manipulation, parsing, and to find/manipulate specific text within larger strings.

## How to:

Here's how to do some common substring work in Rust.
```Rust
fn main() {
    let string = "Hello, I am your Rust program!";
    let part = &string[7..24];
    println!("Substring is: {}", part);
}
```
When you run the code:
```Output
Substring is: I am your Rust
```
In Rust, we use indexing or slicing syntax to get a substring. The `7..24` is slicing to extract characters from index `7` to `23` (24 - 1).

## Deep Dive

`Substring` isn't unique to Rust; many languages like Python, Java implement it differently. In Rust, it's more complex due to memory safety - preventing data races.

Alternatives? Well, `String::from` or `String::to_string()`, but these clone the whole string, less efficient.

The implementation in Rust respects the language's focus on ownership and borrowing. Careful: misuse can lead to `StringIndexOutOfRange` or `CannotBorrowMut` errors.

## See Also

Further reading and examples on the topic:
- Rust Documentation: [Strings](https://doc.rust-lang.org/book/ch08-02-strings.html#storing-utf-8-encoded-text-with-strings)
- Rust by Example: [Slice Type](https://doc.rust-lang.org/rust-by-example/primitives/slice.html)