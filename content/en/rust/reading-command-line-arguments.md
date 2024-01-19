---
title:                "Reading command line arguments"
html_title:           "C++ recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?
Command line arguments are inputs given at the runtime of a program. Programmers use them because they allow the user to control the program's behaviour without changing its source code.

## How to:
In Rust, we can access command line arguments by using the `std::env::args()` function. This function returns an iterator over the arguments that were supplied to the program at runtime.

Here's a quick example:

```rust
fn main() {
    for argument in std::env::args() {
        println!("{}", argument);
    }
}
```

Running this program with `rustc main.rs && ./main arg1 arg2 arg3` will yield:

```
./main
arg1
arg2
arg3
```

## Deep Dive
Historically, command line arguments became popular due to the widespread usage of Unix-based systems. They're a common way of parameterizing the execution of programs in many programming languages.

The `std::env::args()` function is the most common way to read command line arguments in Rust. There are, however, alternative ways to do this. For instance, `std::env::args_os()` allows retaining the original platform-specific encoding.

Command line arguments are stored in the environment of the process and are typically passed as an array of strings to the main function. It's important to note that Rust returns these arguments as an `Args` struct implementing `Iterator<Item=String>`, which means you can take advantage of the methods provided by the Iterator trait.

## See Also
- [The Command Line Arguments section of The Rust Programming Language](https://doc.rust-lang.org/book/ch12-01-accepting-command-line-arguments.html)
- [std::env::args function](https://doc.rust-lang.org/std/env/fn.args.html)
- [std::env::args_os function](https://doc.rust-lang.org/std/env/fn.args_os.html)