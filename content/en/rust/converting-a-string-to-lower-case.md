---
title:    "Rust recipe: Converting a string to lower case"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Why
Have you ever needed to handle user input in your Rust program and wanted to make sure it was all in lowercase? Converting a string to lowercase is a common task in programming, and in this blog post, we will explore how to do it in Rust.

## How To
To convert a string to lowercase in Rust, we can use the `to_lowercase` method. Let's take a look at an example:

```Rust
let my_string = "hElLo WoRlD";
let lower_string = my_string.to_lowercase();
println!("{}", lower_string);
```

The `to_lowercase` method returns a new `String` with all the characters converted to lowercase. Running this code will print out `hello world`. Simple, right?

But what about non-ASCII characters? Strings in Rust are encoded in UTF-8, so let's see how `to_lowercase` handles them:

```Rust
let my_string = "Γεια Σου Κόσμε";
let lower_string = my_string.to_lowercase();
println!("{}", lower_string);
```

The output of this code would be `γιεια σου κόσμε` because the `to_lowercase` method follows the Unicode standard for case conversion.

## Deep Dive
Now let's take a deeper look at how `to_lowercase` works. Under the hood, it uses the `UnicodeTables` module to map each character to its lowercase counterpart. This ensures that all characters, including non-ASCII ones, are handled correctly.

It's also worth noting that the `to_lowercase` method returns a new `String` instead of modifying the original one. This is because strings in Rust are immutable by default, so a new string must be created to hold the converted characters.

## See Also
To learn more about strings and their methods in Rust, check out the official Rust documentation:
- [Strings](https://doc.rust-lang.org/std/string/)
- [`to_lowercase` method](https://doc.rust-lang.org/std/string/struct.String.html#method.to_lowercase)