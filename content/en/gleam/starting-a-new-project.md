---
title:                "Gleam recipe: Starting a new project"
programming_language: "Gleam"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Why

Starting a new project can be a daunting task, but it can also be an exciting opportunity to create something new and innovative. With Gleam, you have the power to build powerful and efficient software in a modern and functional language.

## How To

To get started with a new project in Gleam, you first need to make sure you have the Gleam compiler and runtime installed on your machine. You can find detailed instructions for doing this in the Gleam documentation.

Once you have Gleam installed, you can create a new project by running the following command in your terminal:

```Gleam new <project_name>```

This will create a new directory with the given project name and initialize it with all the necessary files and folders for a Gleam project. 

Next, you can navigate into the project directory and open the `gleam.toml` file. This is where you can specify any dependencies or external libraries you may need for your project. You can also customize the project name, version number, and other configuration options in this file.

After configuring your project, you can start writing code in the `src` directory. Gleam follows a standard file structure, with each module having its own file with the `.gleam` extension. Modules can then be imported into other modules with the `import` statement.

Here's an example of a simple `hello_world.gleam` module:

```
fn say_hello() {
  "Hello, world!"
}
```

To compile and run this module, you can use the following commands in your terminal:

```
$ gleam build
$ gleam run
```

The output of running this code should be `Hello, world!` printed to your terminal. Congratulations, you have successfully created and executed your first Gleam project!

## Deep Dive

When starting a new project in Gleam, it's important to have a clear understanding of the language's core concepts, such as modules, functions, types, and pattern matching. These concepts may seem unfamiliar at first, especially if you're coming from an object-oriented language, but they are integral to writing efficient and maintainable code in Gleam.

It's also important to keep in mind that Gleam is a statically typed language, meaning that all types are known at compile time. This can help catch errors early on and ensure that your code is free of many common bugs.

To learn more about the fundamentals of Gleam and how to effectively start a new project, be sure to check out the comprehensive documentation on the Gleam website.

## See Also

- [Gleam website](https://gleam.run/)
- [Gleam documentation](https://gleam.run/book/introduction.html)
- [Gleam on GitHub](https://github.com/gleam-lang/gleam)