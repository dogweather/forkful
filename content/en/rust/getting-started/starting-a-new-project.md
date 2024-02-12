---
title:                "Starting a new project"
aliases:
- en/rust/starting-a-new-project.md
date:                  2024-01-20T18:04:28.935864-07:00
model:                 gpt-4-1106-preview
simple_title:         "Starting a new project"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why?

Starting a new project in Rust means setting up a basic structure so your code has a place to live. Programmers kick off new projects to solve problems, learn, or develop software from scratch.

## How to:

To start a new Rust project, you need Cargo—the Rust package manager. Install Rust and Cargo through the official installer, rustup.

```sh
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

After that, it's a simple command to create a new project:

```sh
cargo new my_project
```

This command conjures up a new directory called 'my_project' with all the necessary files:

- `Cargo.toml`: Your project’s manifest with metadata and dependencies.
- `src`: A directory where your source files live.
- `main.rs`: The main entry point for your program.

Here's how simple your `main.rs` looks after creation:

```rust
fn main() {
    println!("Hello, world!");
}
```

To compile and run your project:

```sh
cd my_project
cargo run
```

And like magic, you'll see the output:

```
   Compiling my_project v0.1.0 (path/to/my_project)
    Finished dev [unoptimized + debuginfo] target(s) in 0.0 secs
     Running `target/debug/my_project`
Hello, world!
```

## Deep Dive

Rust has had its own package manager and build system, Cargo, from the early days. Created around 2013, it's Rust's way of managing projects, dependencies, and builds.

Why is Cargo so great for starting new projects?

- **Consistency**: It creates a standardized project structure.
- **Dependencies**: It manages external libraries with ease.
- **Compilation**: It compiles your code, leverages Rust's safety, and performance features.

Other languages use different tools—Node.js has npm, Ruby has Bundler, and Python has Pip. Cargo is Rust's answer to these and arguably does much more out-of-the-box by including the build system, which others delegate to separate tools, like Grunt or Webpack in the JavaScript ecosystem.

Alternatives to starting projects in Rust? Well, you could craft everything by hand or use IDEs, but why reinvent the wheel when Cargo does the heavy lifting?

## See Also

- The Rust Programming Language Book: https://doc.rust-lang.org/book/
- Rust and Cargo installation guide: https://www.rust-lang.org/tools/install
- Cargo documentation: https://doc.rust-lang.org/cargo/
