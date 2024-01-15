---
title:                "Using regular expressions"
html_title:           "Rust recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Why

Regular expressions are powerful textual pattern matching tools that allow for efficient and flexible string manipulation. In Rust, they are implemented through the standard library's `regex` crate and provide a convenient way to handle complex text parsing and validation tasks. By familiarizing yourself with regular expressions, you can significantly enhance your coding capabilities and streamline your data processing workflows.

## How To

To use regular expressions in your Rust code, first add the `regex` crate to your `Cargo.toml` file, like so:

```Rust
[dependencies]
regex = "1.4.2"
```

Then, import the `Regex` struct from the crate in your code:

```Rust
use regex::Regex;
```

Next, you can create a new `Regex` object by passing in the desired regular expression pattern as a string:

```Rust
let pattern = Regex::new(r"^\w+@[a-z]+\.[a-z]{2,3}$")?; // creates a regex for validating email addresses
```

With your `Regex` object in hand, you can now perform different operations such as matching, replacing, or capturing text based on the given pattern. For example, to check if a string matches the regex, you can use the `is_match` method:

```Rust
let email = "john@example.com";
if pattern.is_match(email) {
    println!("Valid email address!");
} else {
    println!("Invalid email address!");
}
```

You can also capture parts of a string using capture groups and extract the captured text using the `captures` method:

```Rust
let text = "Today is 12/04/2021.";
let date_regex = Regex::new(r"(\d{2})/(\d{2})/(\d{4})").unwrap();
if let Some(captures) = date_regex.captures(text) {
    let day = captures.get(1).unwrap().as_str();
    let month = captures.get(2).unwrap().as_str();
    let year = captures.get(3).unwrap().as_str();
    println!("Date: {}/{}/{}", month, day, year); // prints "Date: 04/12/2021"
}
```

These are just a few examples of how regular expressions can be used in Rust. For a more comprehensive guide, check out the official Rust documentation and the additional resources listed in the "See Also" section below.

## Deep Dive

Regular expressions in Rust follow the Perl-style syntax and support most regex features you would expect, including character classes, quantifiers, anchors, and capturing groups. However, it is worth noting that due to Rust's emphasis on safety, some regex options may be disabled by default to prevent potentially dangerous operations. For more advanced usage, the `RegexBuilder` struct allows you to customize certain settings and enable disabled features.

Additionally, the `regex` crate also provides a `RegexSet` struct that allows you to match against multiple regular expressions simultaneously for more complex scenarios. Moreover, the `replace` and `replacen` methods offer different ways to replace text based on a given regex pattern and substitution string.

## See Also

- Rust documentation: [Regular Expressions](https://doc.rust-lang.org/std/str/struct.Regex.html)
- Rust By Example: [Regular Expressions](https://rustbyexample.com/std/regex.html)
- Official Regex Syntax Guide: [Regular Expressions - Syntax](https://doc.rust-lang.org/regex/regex/index.html#syntax)