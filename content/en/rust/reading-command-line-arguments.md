---
title:    "Rust recipe: Reading command line arguments"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Why

Do you ever find yourself needing to run a program with custom user input from the command line? This is a common situation for many software developers, and understanding how to read command line arguments is a crucial skill for any programmer. In this blog post, we will explore how to read command line arguments in Rust and why it is an important concept to know.

## How To

To read command line arguments in Rust, we can use the standard library's `args()` function. This function returns an iterator over the command line arguments passed to the program.

Here is a simple example of how to use `args()` in a Rust program:

```Rust
use std::env; // Import the standard library's environment module

fn main() {
    // Get the iterator over the command line arguments
    let mut args = env::args();

    // Skip the first argument, which is the name of the program
    args.next();

    // Iterate over the remaining arguments and print them out
    for arg in args {
        println!("{}", arg);
    }
}
```

If we compile and run this program with the command `rustc main.rs` followed by `./main hello world`, the output will be:

```
hello
world
```

We can also access specific arguments by index using the `nth()` method. For example, if we want to access the second argument, we can use `args.nth(1)`.

## Deep Dive

Command line arguments are a way for users to pass input to a program at runtime. They are useful for providing custom configurations or data without the need to recompile the program. 

In Rust, the `args()` function returns an iterator over `String` values. This means that each argument is stored as a `String` type, which can be manipulated and processed like any other `String` in Rust.

One important thing to note is that the `nth()` method returns an `Option<String>` type, meaning it can either contain a `String` value or be `None`. This is because the index may not exist if the user does not provide enough arguments. It is important to handle this possibility in our code to avoid errors.

## See Also

Here are some links to learn more about reading command line arguments in Rust:

- [Rust Standard Library Documentation on `std::env`](https://doc.rust-lang.org/std/env/index.html)
- [The Rust Programming Language book - Command Line Arguments](https://doc.rust-lang.org/book/ch12-03-improving-error-handling-and-modularity.html#command-line-arguments)
- [Command Line Interfaces in Rust - YouTube tutorial](https://www.youtube.com/watch?v=QgZGj_DbdHg)