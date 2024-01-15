---
title:                "Deleting characters matching a pattern"
html_title:           "Rust recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why

Deleting characters that match a specific pattern can be incredibly useful when working with strings or in text processing. It allows for efficient manipulation of large amounts of data and can simplify tasks such as data cleaning or formatting.

## How To

To delete characters matching a pattern in Rust, we can use the `replace()` function from the standard library. This function takes in three parameters: the original string, the pattern to match, and the replacement string. Here's an example:

```Rust
let original = "Hello, World!";
let modified = original.replace(",", "");
```

This code will replace all commas in the original string with an empty string, effectively deleting them. The resulting `modified` string will now be "Hello World!".

We can also use regular expressions to match more complex patterns. Rust provides the `regex` crate, which allows us to use regular expressions in our code. Here's an example:

```Rust
use regex::Regex;

let original = "The quick brown fox jumps over the lazy dog.";
let regex = Regex::new("[aeiou]").unwrap();
let modified = regex.replace_all(original, "");
```

In this code, we are using the `replace_all()` function from the `Regex` struct to replace all vowels in the original string with an empty string. The resulting `modified` string will be "Th qck brwn fx jmps vr th lzy dg.".

## Deep Dive

The `replace()` and `replace_all()` functions in Rust use a technique called "in-place replacement". This means that the original string is mutated instead of creating a new string. This can be more efficient in terms of memory usage, especially when working with large strings.

It's worth noting that both functions return a `String` type, which is a dynamically allocated string. This means that the original string must be copied to a new location in memory, even though the mutation is done in-place. This is something to keep in mind when dealing with memory-sensitive applications.

## See Also

Here are some additional resources for working with patterns and string manipulation in Rust:

- [Rust String documentation](https://doc.rust-lang.org/std/string/struct.String.html)
- [Regex crate documentation](https://docs.rs/regex/1.4.3/regex/)
- [Official Rust website](https://www.rust-lang.org/)