---
title:                "Rust recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Why

Regular expressions are a powerful tool for text processing and pattern matching in various programming languages. Rust, being a modern systems programming language, also has built-in support for regular expressions. It allows developers to easily search and manipulate strings with complex patterns, making it a valuable skill to have in your programming arsenal.

## How To

To use regular expressions in Rust, you first need to import the `regex` crate in your code. You can do this by adding the following line to your `Cargo.toml` file:

```Rust
[dependencies]
regex = "1.5.4"
```

Next, you can use the `Regex` struct provided by the `regex` crate to create a regular expression object. You can then use this object to match patterns against a given string. Let's see an example of this in action:

```Rust
use regex::Regex;

fn main() {
  let re = Regex::new(r"hello.*world").unwrap();
  let text = "hello dear world";

  if re.is_match(text) {
    println!("Match found!");
  } else {
    println!("No match found :(");
  }
}
```

In this example, we create a `Regex` object that would match any string starting with "hello" and ending with "world". We then use the `is_match` method to check if our `text` variable passes this test. As you can see, using regular expressions in Rust is pretty straightforward.

## Deep Dive

Aside from basic pattern matching, regular expressions in Rust also support capturing groups, replacement, and more advanced features such as lookarounds. The `regex` crate provides a comprehensive API for working with regular expressions, including methods for finding multiple matches, iterating over captured groups, and replacing matched patterns with new strings.

One important thing to note is that Rust's regular expressions use the PCRE (Perl Compatible Regular Expressions) syntax. So if you're already familiar with regular expressions in other languages, you should feel right at home in Rust.

## See Also

If you want to learn more about using regular expressions in Rust, here are some helpful resources:

- The Rust `regex` crate documentation: https://docs.rs/regex/1.5.4/regex/
- The Rust Book chapter on regular expressions: https://doc.rust-lang.org/book/ch09-06-patterns.html
- An interactive tutorial for learning regular expressions in Rust: https://learnbyexample.github.io/Rust_regular_expressions/