---
title:    "Rust recipe: Starting a new project"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/rust/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Why

Starting a new project in Rust can be an exciting endeavor, especially for those who are new to the language. Rust is a modern, safe, and high-performance language that brings a unique approach to systems programming. By starting a new project in Rust, you can learn a valuable skill while also creating efficient and reliable software.

## How To

To start a new project in Rust, follow these simple steps:

1. Install Rust: If you don't already have Rust installed, you can download and install it from the official Rust website. Rust also comes with a package manager called Cargo, which will be used to create and manage your project.

2. Create a new project: Open your terminal and navigate to the directory where you want to create your project. Then, run the command `cargo new <project_name>`. This will create a new directory with the given project name and generate some starter code for you to work with.

3. Write some code: Open the project directory in your preferred code editor and start coding. Rust uses the `.rs` file extension for its source code files. You can code in these files using the Rust syntax and conventions.

4. Build and run your project: To build your project, run the command `cargo build` in your project directory. This will compile your code and generate an executable file. To run your project, use the command `cargo run`.

Here is a simple "Hello, world!" program written in Rust:
```Rust
fn main() {
    println!("Hello, world!");
}
```

Running it will output:
```
Hello, world!
```

## Deep Dive

Starting a new project in Rust also involves understanding some important concepts such as ownership, borrowing, and lifetimes. Rust's strict ownership rules ensure memory safety and prevent common errors such as dangling pointers or data races.

Another key aspect of Rust is its package manager, Cargo. Cargo takes care of project dependencies, building, and testing, making it easy to manage complex projects.

To learn more about these concepts and how they work in Rust, check out the references in the "See Also" section below.

## See Also

- [The Rust Programming Language Book](https://doc.rust-lang.org/book/)
- [Rust By Example](https://doc.rust-lang.org/stable/rust-by-example/)
- [Official Rust Website](https://www.rust-lang.org/)