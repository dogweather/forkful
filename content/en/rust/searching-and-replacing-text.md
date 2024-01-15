---
title:                "Searching and replacing text"
html_title:           "Rust recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why
Have you ever found yourself manually changing the same word or phrase throughout a large codebase? Sure, you could use find and replace, but what about if you only want to replace certain instances or words? This is where searching and replacing text in Rust can come in handy.

## How To
To search and replace text in Rust, you can use the `replace` function from the standard library's `str` module. Let's take a look at an example:

```Rust
let text = "Hello, world!";
let new_text = text.replace("world", "Rust");
println!("{}", new_text);
```

This code will search for the word "world" in the `text` string and replace it with "Rust". The output will be "Hello, Rust!".

But what if you only want to replace certain instances or words? You can use regular expressions with the `replace_all` function. Let's see how this might look:

```Rust
use regex::Regex;
let text = "The quick brown fox jumps over the lazy dog";
let regex = Regex::new("the").unwrap();
let new_text = regex.replace_all(text, "a");
println!("{}", new_text);
```

This code will search for all instances of the word "the" and replace them with "a". The output will be "a quick brown fox jumps over a lazy dog".

## Deep Dive
The `replace` and `replace_all` functions are actually wrappers for an underlying function, `replace_range`. This function takes in a range of indices to replace instead of searching for a specific word or phrase. You can also use `replace_range` to insert text at a specific index.

```Rust
let mut text = String::from("rust programming");
let new_text = String::from("Rust programming");
text.replace_range(..1, "R");
println!("{}", text); // will output "Rust programming"
text.replace_range(4..5, "");
println!("{}", text); // will output "Rst programming"
text.replace_range(4..4, "Prog");
println!("{}", text); // will output "Rst Prog ramming"
```

As you can see, the `replace_range` function can be useful for more advanced text manipulation.

## See Also
- [Rust Standard Library Documentation](https://doc.rust-lang.org/std/str/fn.replace.html)
- [Regular Expressions in Rust](https://doc.rust-lang.org/book/ch09-02-references-and-borrowing.html)