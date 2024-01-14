---
title:                "Rust recipe: Reading command line arguments"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why

As programmers, we encounter various types of applications that require us to interact with the command line. Whether it's passing in user input or setting up configurations, understanding how to work with command line arguments is an essential skill. In this blog post, we will explore how to read command line arguments in Rust, with detailed and easy-to-follow steps.

## How To

To read command line arguments in Rust, we will use the `std::env` module, which provides functions for accessing command line arguments. Let's start by creating a new Rust project and declaring our dependencies:

```rust
use std::env;
```

Next, we can access the command line arguments using the `args()` function, which returns an iterator of type `Args`:

```rust
let args: Vec<String> = env::args().collect();
```

The `args()` function will return all command line arguments as strings, including the name of the executable itself. We can use the `argn()` function to retrieve a specific argument. For example, if we want to retrieve the first argument, we can use:

```rust
let first_arg = match args.get(1) {
    Some(arg) => arg,
    None => panic!("No arguments provided"),
};

// Print the first argument
println!("The first argument is: {}", first_arg);
```

Alternatively, we can also use the `argn_os()` function to return arguments as `OsStrings`, which can then be converted into `String` objects. This can be useful when working with non-UTF8 arguments. Now, let's run our program with some arguments and see the output:

```
$ cargo run arg1 arg2 arg3
The first argument is: arg1
```

## Deep Dive

One important thing to note when working with command line arguments is that they are separated by spaces. This means that if we have a single argument that contains spaces, it will be split into multiple arguments. For example, if we run our program with the argument `"Welcome to Rust"`, it will be treated as three separate arguments: `"Welcome", "to", "Rust"`.

Additionally, we can use the `env::args_os()` function to retrieve command line arguments as `OsStrings` without potentially splitting them. This can be useful for applications that need to pass arguments with spaces or special characters.

Another useful function provided by the `std::env` module is `current_dir()`, which returns the current working directory as a `PathBuf` object. This can be helpful when creating relative paths for file operations within our application.

## See Also

To learn more about working with command line arguments in Rust, check out these resources:

- [Rust Standard Library documentation](https://doc.rust-lang.org/std/env/index.html)
- [Rust By Example: Command Line Arguments](https://doc.rust-lang.org/stable/rust-by-example/std_misc/arg.html)
- [How to Read Command Line Arguments in Rust](https://www.aidanwilson.dev/how-to-read-command-line-arguments-in-rust)

Now that you have a good understanding of how to read command line arguments in Rust, you can confidently build robust and versatile command line applications. Happy coding!