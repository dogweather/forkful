---
title:                "Capitalizing a string"
html_title:           "Rust recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Capitalize Strings in Rust

## What & Why?

Capitalizing strings is making all the first letters of the words within the string uppercase. It makes your program user interface tidy and readable.

## How To:

Take a look at this simple way to capitalize a string in Rust.  

```Rust
fn capitalize(s: &str) -> String {
    let mut cap = String::new();
    let mut new_word = true; 
    for c in s.chars() {
        if c.is_alphanumeric() {
            if new_word { cap.extend(c.to_uppercase()); new_word = false; }
            else { cap.push(c); }
        } else { cap.push(c); new_word = true; }
    }
    cap
}

fn main() {
    println!("{}", capitalize("hello, rust programming!")); //Output: "Hello, Rust Programming!"
}
```
In this code snippet, `capitalize()` is a utility function that receives a str (slice of a string) and returns a new string that includes capitalized characters according to the rules defined by the logic enclosed within the loop.

## Deep Dive

Historical context: Rust doesn't come with a built-in method to capitalize strings - that's why we need a custom function as above. 

Alternatives: There may be alternative libraries/add-ons providing this functionality such as the popular `heck` crate in Rust that gives solution for string case conversions. 

Implementation details: The function above checks for alphabetic characters to capitalize the first letter after each non-alphabetic character (indicating a new word). Uppercased characters use more memory than lowercase, so this version avoids unnecessary creations of uppercase characters that aren't needed. Only the first character of each word is uppercased, reducing storage and computation.

## See Also

Heck crate: https://crates.io/crates/heck
Guide to Strings in Rust: https://stevedonovan.github.io/rustifications/2018/09/08/common-rust-lifetime-misconceptions.html#str-is-a-view-into-a-string