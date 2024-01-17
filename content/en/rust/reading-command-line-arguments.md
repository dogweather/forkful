---
title:                "Reading command line arguments"
html_title:           "Rust recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?

Reading command line arguments is the process of accepting user inputs from the command line when running a program. This is a common practice among programmers to enable their programs to be customizable and dynamic based on user input.

## How to:

To read command line arguments in Rust, we can use the ```args()``` function from the ```std::env``` module. This function returns an iterator that provides the command line arguments as strings. We can then iterate through the arguments and perform any necessary operations.

Example:

```
use std::env;

fn main() {
    let arguments: Vec<String> = env::args().collect();
    for argument in arguments {
        println!("{}", argument); // prints out each command line argument
    }
}
```

Sample output:

```
> rustc main.rs
> ./main hello world 123
./main
hello
world
123
```

## Deep Dive:

Command line arguments have been used since the early days of computer programming, where they were necessary for running programs on command-line-only operating systems. In modern programming, command line arguments are still used for the same reasons, but they have also been replaced by more user-friendly graphical user interfaces in many cases.

An alternative to reading command line arguments in Rust is the ```clap``` crate, which provides a more robust and structured way of handling command line arguments. However, for simple programs, using the ```args()``` function should suffice.

It is worth noting that command line arguments are not limited to strings, but can be any data type that can be parsed from a string. For example, using the ```parse()``` method on a string, we can convert it to a desired data type such as integer or float.

## See Also:

- [The Rust Standard Library](https://doc.rust-lang.org/std/#command-line-arguments)
- [The clap crate](https://docs.rs/clap/2.33.1/clap/): Alternative for handling command line arguments in Rust