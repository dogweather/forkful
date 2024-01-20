---
title:                "Deleting characters matching a pattern"
html_title:           "Lua recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Deleting Characters Matching a Pattern in Rust

## What & Why?

Deleting matching characters is the process of removing all instances of certain characters in a string. Programmers do this to sanitize, parse, or simplify data.

## How to:

In Rust, the built-in `replace` method can do this trick. Use an empty string as the replace argument. Here's how to delete all `'a'` characters from a string:

```Rust
fn main() {
    let str = "banana";
    let result = str.replace("a", "");
    println!("{}", result);
}
```
Output:
```
bnn
```

To delete characters matching one of many patterns, use a `Regex`. Here's how:

```Rust
use regex::Regex;

fn main() {
    let re = Regex::new("[aeiou]").unwrap();
    let str = "banana";
    let result = re.replace_all(&str, "");
    println!("{}", result);
}
```

Output:
```
bnn
```

## Deep Dive

History of deleting characters in programming started with early sanitizing needs. In Rust, the `replace` and `Regex::replace_all` methods are built-in ways, but your own methods are possible, too.

`replace` is faster, easier, but simpler. `Regex::replace_all` needs the `Regex` crate but handles more complex patterns. Performance-wise, `replace` is faster for simple situations, `Regex::replace_all` is better for complex patterns.

These methods work by scanning the string for either the exact match (for `replace`) or the RegEx pattern match (for `replace_all`), then replacing every match with the specified pattern. 

## See Also

[The Rust Regex Documentation](https://docs.rs/regex/1.5.4/regex/): for understanding how to use Rust's Regex crate.
[Rust's String Documentation](https://doc.rust-lang.org/std/string/struct.String.html): to know other manipulatory methods like `replace`. 

Avoid using a sledgehammer to crack a nut: not all problems need RegEx. Know when to use `replace` and `Regex::replace_all`. Happy coding!