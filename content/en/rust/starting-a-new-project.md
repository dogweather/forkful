---
title:                "Starting a new project"
html_title:           "Bash recipe: Starting a new project"
simple_title:         "Starting a new project"
programming_language: "Rust"
category:             "Rust"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why?

Starting a new project is the initial step of creating a new, unique application from ground zero. Programmers do this to design new functionality, solve a problem, or take an idea and bring it to life.

## How to:

In Rust, you'll use Cargo, Rust's built-in package manager, to create and manage your projects. Here's how:

```Rust
// To create a new Rust project
$ cargo new my_cool_project
Created binary (application) `my_cool_project` package

// Check out the directory it created
$ cd my_cool_project
```

Your directory should look like this:

```Rust
my_cool_project
 ├── Cargo.toml
 └── src
     └── main.rs
```

Cargo.toml is your project build config and src/main.rs is a basic 'Hello, World!' application. Now, run the project:

```Rust
$ cargo run
   Compiling my_cool_project v0.1.0 (/my_cool_project)
    Finished dev [unoptimized + debuginfo] target(s) in 0.31s
     Running `target/debug/my_cool_project`
Hello, world!
```
The 'Hello, World!' message is a success! You're up and running.

## Deep Dive

Historically, Rust and therefore Cargo, came into being to solve complex system issues with a focus on safety and performance. Before Cargo, managing a Rust project was chaotic—managing dependencies and building a project robbed you of time and energy before you even got to your code.

Key alternatives to starting a project, like cloning a repo or reusing code from another project, lack the freshness and learning experience of a new project. Moreover, they may come wired with old bugs or unwanted features.

Your new project creation uses templates and defaults as described in the Cargo.toml file. It's the heart of your Rust application. It defines your project details (like its name and version), its dependencies, and build instructions. This declarative approach, coupled with Cargo's functionality of downloading and building your dependencies, simplifies working with Rust to a large degree.

## See Also

- Details on Cargo.toml and how to customize it: https://doc.rust-lang.org/cargo/reference/manifest.html
- A full list of Cargo's commands: https://doc.rust-lang.org/cargo/commands/index.html
- From newbie to professional Rustacean: The Rust Programming Language book: https://doc.rust-lang.org/book/.