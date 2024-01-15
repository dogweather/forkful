---
title:                "Starting a new project"
html_title:           "Gleam recipe: Starting a new project"
simple_title:         "Starting a new project"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Why
Starting a new project can be daunting, but with the right programming language, it can be an exciting and rewarding experience. This is where Gleam comes in - a modern, statically typed language designed specifically for building robust and scalable applications.

## How To
To start a new project in Gleam, you'll need to follow a few simple steps:

1. Install the Gleam compiler by following the instructions on their [website](https://gleam.run/getting-started/).
2. Create a new project directory and navigate to it.
3. Initialize your project using the `gleam init` command, which will set up the necessary boilerplate code and folder structure for your project.
4. Once your project is initialized, you can start writing code in the `src` directory. Gleam follows the familiar module-based system, so you'll need to create a `main.gleam` file and start writing code in it.

Here's an example of a simple "Hello, World!" program in Gleam:

```Gleam
import gleam/io

fn main() {
    io.print("Hello, World!")
}
```

To run this code, you'll need to compile it first using the `gleam build` command, which will generate an executable file. Then, simply run the executable to see the output.

## Deep Dive
When starting a new project in Gleam, there are a few things to keep in mind:

- Gleam's strong type system ensures that your code is robust and error-free, making debugging much easier.
- It has a simple and concise syntax, making it easy to pick up for developers of all levels.
- Gleam follows functional programming principles, which promotes code reuse and maintainability.
- The language is still in its early stages, so documentation and community support may be limited.

To dive deeper into learning Gleam, check out their [official guide](https://gleam.run/book/getting_started.html) and [GitHub repository](https://github.com/gleam-lang/gleam).

## See Also
- [The official Gleam website](https://gleam.run/)
- [Gleam on GitHub](https://github.com/gleam-lang/gleam)