---
title:                "Rust recipe: Using regular expressions"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Why

Regular expressions are an essential tool for programmers, allowing for advanced text pattern matching and manipulation. In Rust, regular expressions are particularly useful for tasks such as input validation, data parsing, and text searching.

## How To

To use regular expressions in Rust, we first need to import the `regex` crate. This can be done by adding the following to our `Cargo.toml` file:

```
[dependencies]
regex = "1.4.2"
```

Next, we will need to use the `Regex` struct from the `regex` crate. This allows us to create a regular expression object by passing in a string representation of the pattern we want to match.

Let's say we want to find all instances of the word "rust" in a given string. We can do this using the `regex::RegexBuilder` struct, which allows us to customize our regex pattern. Our code will look like this:

```
use regex::RegexBuilder;

fn main() {
    let regex = RegexBuilder::new("rust")
        .case_insensitive(true)
        .dot_matches_new_line(true)
        .build()
        .unwrap();

    let text = "Rust is a systems programming language";

    for match in regex.find_iter(text) {
        println!("Found match: {}", match.as_str());
    }
}
```

The first line of our code creates a new `RegexBuilder`, which takes in our pattern as a parameter. We then set two options - `case_insensitive` and `dot_matches_new_line` - to ensure that our regex is case-insensitive and can match across multiple lines.

Next, we use the `find_iter` method to iterate through all the matches found in our text. In this case, the output will be:

```
Found match: Rust
```

## Deep Dive

In Rust, regular expressions are powered by the "rust-lang/regex" crate, which uses the popular "Oniguruma" regular expression library. This provides a powerful and efficient implementation of regular expressions in Rust.

To build more complex regex patterns, we can use a combination of special characters and text to create a specific search pattern. Some commonly used special characters include `.` to match any character, `\d` to match any digit, and `|` to match either side of the `|` symbol.

It's important to keep in mind that regular expressions can be resource-intensive, so it's important to use them wisely and test them thoroughly before implementing them in production code.

## See Also

For more information on using regular expressions in Rust, check out the official Rust documentation and the "rust-lang/regex" crate repository:

- [Rust Documentation](https://doc.rust-lang.org/regex)
- [regex Crate Repository](https://github.com/rust-lang/regex)