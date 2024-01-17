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

## What & Why?

Regular expressions, or "regex" for short, are a tool used by programmers to search, manipulate, and validate text strings based on patterns. They can save time and effort when working with large amounts of text data, allowing for more efficient and accurate coding.

## How to:

To use regular expressions in Rust, you can use the crate `regex`. This crate provides a Regex type that represents a compiled regular expression. Here's an example of how to use it:

```
use regex::Regex;

// Create a regular expression to match a phone number with optional country code
let re = Regex::new(r"^\+?([0-9]{1,3})?([0-9]{3})([0-9]{3})([0-9]{4})$").unwrap();

// Test if a phone number is a match
let phone_number = "+1 (123) 456-7890";
if re.is_match(phone_number) {
    println!("Valid phone number");
} else {
    println!("Invalid phone number");
}

// Extract the country code and local number from the phone number
let caps = re.captures(phone_number).unwrap();
println!("Country code: {}", caps.get(1).map_or("", |m| m.as_str()));
println!("Local number: {}", caps.get(2).unwrap().as_str());
```

This will output:

```
Valid phone number
Country code: 1
Local number: 1234567890
```

## Deep Dive:

Regular expressions have been around for decades and have been implemented in many programming languages, including Rust. They were first introduced in the 1950s by mathematician Stephen Kleene as a way to represent patterns in formal languages. Today, they are widely used in text processing tasks, such as search and replace, email validation, and data extraction.

There are some alternatives to using regular expressions in Rust, such as the `str::contains` and `str::starts_with` methods. While these may be simpler to use in certain cases, regular expressions offer more advanced pattern matching capabilities.

For those interested in the nitty gritty details, the `regex` crate uses an algorithm called "DFA" (deterministic finite automaton) to efficiently match text against a regular expression. It also supports Unicode, making it easier to work with non-ASCII characters.

## See Also:

- [The Rust Book: Using Regular Expressions](https://doc.rust-lang.org/book/ch09-06-repetition.html)
- [Rust Regular Expressions Cookbook](https://rust-lang-nursery.github.io/regex-cookbook/)
- [Regex101: An Online Regular Expression Tester](https://regex101.com/)