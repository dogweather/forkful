---
title:                "Using regular expressions"
html_title:           "Bash recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Regular expressions, or regex for short, are sequences of characters forming search patterns. Programmers use regex to search, edit, or manipulate text by matching complex patterns, often for validation or parsing purposes.

## How to:
Rust uses the `regex` crate for regex operations. First, add it to your `Cargo.toml`:

```toml
[dependencies]
regex = "1"
```

Then, you can match strings like so:

```rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"^\d{4}-\d{2}-\d{2}$").unwrap();
    let date = "2023-04-01";

    println!("Does the text match the date pattern? {}", re.is_match(date));
}
```

Output:

```
Does the text match the date pattern? true
```

For capturing groups:

```rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"(\w+)@(\w+)\.(\w+)").unwrap();
    let email = "user@example.com";

    match re.captures(email) {
        Some(caps) => {
            println!("User: {}, Domain: {}, Extension: {}", &caps[1], &caps[2], &caps[3]);
        }
        None => println!("No match found."),
    }
}
```

Output:

```
User: user, Domain: example, Extension: com
```

## Deep Dive
Regex has been around since the 1950s, with roots in automata theory and formal language. Rust's `regex` module is built for speed and safety, focusing on compiling efficient regex patterns at runtime. Alternatives to regex include string functions like `find`, `split`, and `replace`, which cover simpler use cases without patterns. Regexes in Rust are particularly efficient due to extensive optimization and compilation of the regex patterns.

## See Also
- The `regex` crate documentation: https://docs.rs/regex/
- Rust's book section on regex: https://doc.rust-lang.org/book/ch18-00-patterns.html
- Regular Expressions chapter of "The Rust Programming Language": https://doc.rust-lang.org/stable/book/ch18-03-pattern-syntax.html