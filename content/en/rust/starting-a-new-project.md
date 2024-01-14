---
title:                "Rust recipe: Starting a new project"
programming_language: "Rust"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Why
Rust is a modern, powerful and safe programming language that has gained a lot of popularity in recent years. Given its unique features and performance, it is a great choice for anyone looking to start a new project.

## How To
To get started with Rust, you will first need to install it on your system. Once that's done, you can use the `cargo` command to create a new project. Let's see an example of creating a simple "Hello World" program in Rust:

```Rust
fn main() {
    println!("Hello, world!");
}
```

Here, the `fn` keyword is used to define a function named `main` which is the entry point of our program. The `println!()` macro is used to print the string "Hello, world!" to the console. Now, let's compile and run our program using the `cargo` command:

```Rust
cargo run
```

This will compile and execute our program, and you should see the output "Hello, world!" displayed on the console.

## Deep Dive
Starting a new project in Rust involves understanding some key concepts such as crates and modules. Crates are the basic unit of compilation in Rust, while modules allow you to organize your code into smaller, reusable units. You can use the `use` keyword to import modules or external crates into your project.

Another important aspect of Rust is its strong type system. This ensures that variables and functions are properly defined and used, reducing the chances of runtime errors. Rust also has a built-in package manager, `cargo`, which handles dependencies, builds, and testing for your project.

## See Also
- [The Rust Programming Language](https://www.rust-lang.org/)
- [Official Rust Documentation](https://doc.rust-lang.org/)
- [Rust subreddit](https://www.reddit.com/r/rust/)