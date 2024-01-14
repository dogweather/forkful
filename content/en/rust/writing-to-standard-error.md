---
title:                "Rust recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why 
Why Write to Standard Error in Rust?

Rust is a modern, safe, and efficient programming language that has gained popularity in recent years. With its strong type system and powerful memory management, it is a great choice for building reliable and high-performance software. One common task in Rust programming is writing to standard error, also known as stderr. In this blog post, we will explore why someone would want to write to stderr and how to do it in Rust.

## How To
To write to standard error in Rust, we can use the `eprintln!` macro. This macro works similarly to the `println!` macro, but instead of printing to standard output, it prints to standard error. Let's see an example:

```Rust
fn main() {
    eprintln!("This is an error message.");
}
```

Running this code will produce the following output:

```
This is an error message.
```

We can also use string formatting with the `eprintln!` macro, just like we do with the `print!` and `println!` macros. Here's an example:

```Rust
fn main() {
    let name = "John";
    eprintln!("Hello, {}!", name);
}
```

This will output:

```
Hello, John!
```

Writing to standard error can be useful for debugging purposes or for displaying important error messages to the user.

## Deep Dive 
When writing to standard error in Rust, we are actually writing to a special file descriptor known as stderr. This file descriptor is used to display error messages separately from normal program output. By default, stderr is displayed on the terminal, but it can also be redirected to a file using shell commands.

One thing to note is that writing to stderr is not the same as throwing an error. In Rust, errors are handled through the use of the `Result` type and the `Err` variant. This allows for more control and better error handling in Rust programs. Writing to stderr is just one way to display information to the user or the developer during program execution.

## See Also 
- [Rust documentation on std::io::Error](https://doc.rust-lang.org/std/io/struct.Error.html)
- [Rust documentation on writing to stderr](https://doc.rust-lang.org/std/io/fn.stderr.html)
- [Rust by Example: stderr](https://doc.rust-lang.org/rust-by-example/hello/print/print_debug.html)