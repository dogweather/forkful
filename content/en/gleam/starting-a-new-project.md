---
title:    "Gleam recipe: Starting a new project"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Why

Are you tired of the limitations and bugs in your current programming language? Want to explore a new, powerful and modern language? Look no further than Gleam - the functional and statically typed language designed for writing robust and scalable applications. With its simple syntax and powerful type system, Gleam is gaining popularity among developers as a go-to language for building reliable and performant software. In this blog post, we'll explore how to get started with Gleam, so you can start your own exciting project in no time!

## How To

The first step to getting started with Gleam is to install the Gleam compiler, which can be done easily using the `brew` package manager on Mac or by following instructions for other systems on the [official Gleam website](https://gleam.run/getting-started/). Once installed, you can create a new project using the `gleam new` command and specify a name for your project. This will generate a basic project structure with all the necessary files to get started. Let's take a look at a simple "Hello World" program in Gleam:

```Gleam
// Import the standard library's stdout function
import gleam/io

// Define a main function that takes no arguments
pub fn main() {
    // Call the stdout function to print "Hello World"
    io.stdout("Hello World")
}
```

After saving this code in a file with a `.gleam` extension, you can compile and run it using the `gleam build` and `gleam run` commands respectively. This should print "Hello World" in your console, demonstrating the simple yet powerful nature of Gleam.

## Deep Dive

Gleam is a statically typed language, which means that all types are checked at compile time, ensuring a high level of reliability and performance. The language uses structural typing, meaning that a type only needs to have the same shape as another type to be considered compatible. This allows for flexible and composable types, making it easier to express complex data structures in a concise manner.

In addition, Gleam also has a strong focus on concurrency, with built-in features for creating lightweight and efficient processes. This makes it easier to write highly concurrent and parallel applications without the need for external tools or libraries.

## See Also

To learn more about Gleam and its features, check out the official [documentation](https://gleam.run/documentation/) and the [GitHub repository](https://github.com/gleam-lang/gleam). You can also join the friendly community of Gleam developers on [Discord](https://discord.gg/vJHjZtD) and share your projects or ask for help from experienced users. Happy coding!