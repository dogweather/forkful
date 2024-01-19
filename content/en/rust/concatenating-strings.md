---
title:                "Concatenating strings"
html_title:           "PHP recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?
String concatenation is the process of joining two or more strings end-to-end. Programmers use it to build sentences, messages, or any kind of text data that requires string assembly.

## How to:

Rust provides multiple ways to concatenate strings. 

The first way is by using the `+` operator:

```Rust
let hello = "Hello".to_string();
let world = " World!".to_string();
let hello_world = hello + &world;
println!("{}", hello_world); // prints: Hello World!
```

The other way is to use the `format!` macro:

```Rust
let hello = "Hello";
let world = " World!";
let hello_world = format!("{}{}", hello, world);
println!("{}", hello_world); // prints: Hello World!
```

## Deep Dive

1. **Historical Context**: In early programming languages, string concatenation was usually achieved through library functions. Modern languages, like Rust, have operators and standard library methods for it.
2. **Alternatives**: Besides `+` and `format!`, Rust also provides `push_str` and `push` methods to append strings.
3. **Implementation Details**: When you use `+`, Rust compiler performs a `to_string` conversion behind the scenes. It's important to note that `+` consumes the original variable, while `format!` keeps the originals intact.

```Rust
let hello = "Hello".to_string();
let mut world = " World!".to_string();
world.push_str(&hello);
println!("{}", world); // prints: World!Hello
```

## See Also

- Official Rust documentation about [Strings](https://doc.rust-lang.org/book/ch08-02-strings.html)
- Blog post: [Rust Strings Explained](https://fasterthanli.me/articles/a-half-hour-to-learn-rust)
- Rustlings course on [Strings](https://github.com/rust-lang/rustlings)