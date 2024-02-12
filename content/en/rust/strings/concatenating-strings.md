---
title:                "Concatenating strings"
aliases: - /en/rust/concatenating-strings.md
date:                  2024-01-20T17:35:28.360233-07:00
model:                 gpt-4-1106-preview
simple_title:         "Concatenating strings"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?
String concatenation is sticking strings end-to-end to make a new one. We do it to construct messages, generate output, or work with text dynamically.

## How to:
Rust gives you a few ways to piece texts together. Let’s dig in.

### Using `+` Operator
```Rust
let hello = "Hello".to_string();
let world = " world!";
let result = hello + world;
println!("{}", result); // Output: Hello world!
```
The `+` sticks `" world!"` onto `"Hello"`, but watch out, `hello` needs to be a `String`, not a slice.

### The `format!` Macro
```Rust
let mood = "happy";
let message = format!("Have a {} day!", mood);
println!("{}", message); // Output: Have a happy day!
```
`format!` is like `println!`, mixing variables into text. Super handy for templates.

### Pushing to a String
```Rust
let mut tip = "Remember to".to_string();
tip.push_str(" breathe.");
println!("{}", tip); // Output: Remember to breathe.
```
`push_str` appends a slice to a `String`. Good for adding bits one at a time.

## Deep Dive
String concatenation isn't a new concept. It’s been around since the dawn of programming; after all, we've always needed to mash words together.

In Rust, a `String` is a growable, mutable, owned UTF-8 encoded string type. There are alternatives like `&str`, a string slice, which is a view into a `String`.

Each method has its trade-offs:

- `+` operator is quick for a join or two but devours the left-hand operand (it takes ownership). Every `+` also allocates memory, which can add up.
  
- `format!` doesn’t snatch any owned values, which is polite, but it can be slower due to its flexibility and allocating for each call. It’s your Swiss Army knife for string assembly.

- `push_str` is efficient for a repeated add-on. It doesn't allocate unless the `String` needs more space.

Rust's focus on ownership and borrowing means that it handles strings a bit differently than languages like Python or JavaScript. This difference ensures memory safety but can also come with a learning curve.

## See Also
For a deeper dive:
- Rust Book on Strings: https://doc.rust-lang.org/book/ch08-02-strings.html
- Rust By Example on Strings: https://doc.rust-lang.org/rust-by-example/std/str.html
- Rust std::string::String API Docs: https://doc.rust-lang.org/std/string/struct.String.html
