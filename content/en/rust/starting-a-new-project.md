---
title:                "Starting a new project"
html_title:           "Rust recipe: Starting a new project"
simple_title:         "Starting a new project"
programming_language: "Rust"
category:             "Rust"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Why

So, you've decided to start a new project in Rust - good choice! Rust is a powerful and modern programming language that has been gaining popularity for its speed, safety, and concurrency features. But before you dive into coding, let's take a look at why starting a project in Rust is a great idea.

Rust's strong type system and ownership model help prevent common errors and bugs, making it a reliable choice for building high-performance and secure applications. Its crate ecosystem also offers a vast range of libraries and toolkits, making it easier to integrate with other languages and frameworks.

## How To

To start a new project in Rust, you'll first need to install the Rust toolchain. You can do this by following the installation instructions on the official Rust website. Once installed, you can use the `cargo` command to create a new project with the following command:

```
Rust new my_project
```

This will create a new directory named *my_project* with the basic files and folders needed for a Rust project. Now, let's take a look at an example of a simple "Hello, World!" program in Rust:

```
fn main() {
    println!("Hello, World!");
}
```

Save this code in a file named *main.rs* inside the *src* folder of your project. Then, go to the root folder of your project and run the following command:

```
cargo run
```

You should see the output "Hello, World!" in your terminal. Congratulations! You've just written and executed your first Rust program.

## Deep Dive

Now that you have a basic understanding of how to start a project in Rust and write a simple program, let's dive deeper into the details. Rust follows a strict compile-time checking system and provides comprehensive error messages, making it easy to debug your code. It also has a robust package manager called Cargo, which handles dependency management, building, and running your project.

Rust's syntax is similar to other curly-bracket languages like C and Java, but it has some unique features, such as pattern matching and iterators, which make it more expressive and efficient. Additionally, Rust has a strong community that is always willing to help and support new developers, making it a welcoming environment for beginners.

## See Also

- [Official Rust Website](https://www.rust-lang.org)
- [The Rust Programming Language Book](https://doc.rust-lang.org/book)
- [The Rust Community Forum](https://users.rust-lang.org)