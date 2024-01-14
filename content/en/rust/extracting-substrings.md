---
title:    "Rust recipe: Extracting substrings"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why

One of the most fundamental tasks in string manipulation is extracting substrings. This process involves taking a small portion of a larger string and manipulating it separately. Extracting substrings can be useful in a variety of scenarios, such as parsing data or formatting strings for display.

## How To

In Rust, there are a few different ways to extract substrings from a string. Let's look at some examples using the built-in `String` type.

First, we can use the `slice` method to extract a substring from a specific range of indices in the string:

```Rust
let my_string = String::from("Hello, world!");
let substring = &my_string[0..5]; // Will contain "Hello"
```

Alternatively, we can use the `split_off` method to split the string into two parts, keeping the original string and returning the extracted substring as a new string:

```Rust
let mut my_string = String::from("Hello, world!");
let substring = my_string.split_off(5); // Will return " world!" and modify my_string to only contain "Hello"
```

We can also use regular expressions to extract a substring that matches a specific pattern. This requires the `regex` crate, which can be added to your project's `Cargo.toml` file.

```Rust
use regex::Regex;

let my_string = String::from("Hello, world!");
let re = Regex::new(r"world").unwrap();
let matches: Vec<Match> = re.find_iter(&my_string).collect(); // Will find all instances of "world" in the string
let substring = &my_string[matches[0].start()..matches[0].end()]; // Will contain "world"
```

## Deep Dive

Under the hood, Rust uses byte indexing for strings rather than character indexing. This means that the indices we use to extract substrings must correspond to valid byte indices. For example, if we were to try to extract a substring using the indices `0..3` from the string "नमस्ते" (which contains the Hindi characters for "hello"), we would get an error because those indices do not correspond to valid byte indices for that string.

To get around this, we can use the `char_indices` method to iterate over both the bytes and characters in a string, and use the character indices instead of the byte indices to extract substrings.

```Rust
let my_string = String::from("नमस्ते");
let mut char_indices = my_string.char_indices();
char_indices.next(); // Skips the first byte index, since it does not correspond to a valid character index
let substring = &my_string[char_indices.next().unwrap().0..char_indices.next().unwrap().0]; // Will contain "मस"
```

## See Also

- [Rust String Documentation](https://doc.rust-lang.org/std/string/struct.String.html)
- [Rust Regex Crate](https://crates.io/crates/regex)
- [Unicode Support in Rust](https://doc.rust-lang.org/std/string/struct.String.html#unicode-support)