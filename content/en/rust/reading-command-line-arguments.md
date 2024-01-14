---
title:                "Rust recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why

When writing a command line application, it is important to have the ability to read and parse the arguments provided by the user. This allows for a more interactive and customizable experience for the user, as well as making it easier to handle various inputs and options.

## How To

In Rust, reading command line arguments is a simple process using the `std::env` module. We can access the arguments via the `args()` function, which returns an iterator over the arguments.

Let's take a look at a basic example:

```rust
use std::env;

fn main() {
    // Get the arguments iterator
    let args = env::args();

    // Loop through each argument and print it
    for arg in args {
        println!("{}", arg);
    }
}
```

If we compile and run this code with the command line arguments `rustcli arg1 arg2`, we will get the following output:

```
rustcli
arg1
arg2
```

The first argument always represents the name of the executable file, and the rest are any additional arguments provided by the user.

We can also access specific arguments by using the `nth()` method on the arguments iterator. For example, if we wanted to access the second argument, we could do the following:

```rust
// Get the second argument
let arg2 = env::args().nth(2);

// Print the argument if it exists
if let Some(arg) = arg2 {
    println!("Second argument: {}", arg);
}
```

This is especially useful when dealing with options and flags, as we can check for their presence and take appropriate actions.

## Deep Dive

Along with accessing the arguments, the `std::env` module also provides other useful functions for dealing with command line arguments. For example, we can retrieve the current working directory using the `current_dir()` function, or we can get the path to the executable file using the `current_exe()` function.

We can also use the `var()` function to retrieve environment variables, which can be useful when our application needs to access certain system-specific information.

## See Also

- [The Rust Standard Library Documentation](https://doc.rust-lang.org/std/)
- [The `std::env` module](https://doc.rust-lang.org/std/env/index.html)