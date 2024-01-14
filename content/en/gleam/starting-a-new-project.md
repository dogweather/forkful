---
title:    "Gleam recipe: Starting a new project"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Why
Starting a new programming project can be an exciting and daunting task. It allows you to explore new ideas, solve complex problems, and create something that has the potential to impact others. With Gleam, a functional programming language built for creating scalable and maintainable software, you have the opportunity to take on new challenges and improve your coding skills.

## How To
To get started with Gleam, first make sure you have the latest version of the Gleam compiler installed on your computer. Then, follow these simple steps to begin your project:

  1. Create a new directory for your project and navigate into it.
  2. Initialize a new Gleam project by running `gleam new my_project` in your terminal.
  3. Once the project is initialized, you can open the `my_project` directory in your preferred code editor.
  4. Within this directory, you will find a `src` folder where you can add your Gleam source code files.
  5. To run your project, use the `gleam build` command to compile your code and then `gleam run` to execute it.

Here is a simple `Hello World` example to demonstrate the code structure:

```Gleam
pub fn main() {
  let greeting = "Hello World!"
  io.println(greeting)
}
```

The output when running this code will be `Hello World!` printed to your terminal.

## Deep Dive
When starting a new project with Gleam, there are a few things to keep in mind. Firstly, the language is strongly typed, which means all variables must have a defined type. This may take some getting used to, but it ultimately leads to more robust and bug-free code.

Additionally, Gleam uses the pipe operator `|>` to chain functions together, making code more readable and easier to debug. It also has a powerful type inference system, reducing the amount of type annotations needed.

Another important aspect of starting a new Gleam project is understanding the concept of modules. Modules are used to organize functions and data types, and they can be used to create libraries that can easily be imported into other projects. By breaking your code into modules, you can improve the reusability and maintainability of your codebase.

Lastly, Gleam has a growing community of developers who are always willing to provide support and answer any questions you may have. There are also many helpful resources available, including tutorials, documentation, and sample projects.

## See Also
Here are some useful links to help you get started with Gleam:
- [Gleam Documentation](https://gleam.run/documentation/)
- [Gleam on GitHub](https://github.com/gleam-lang/gleam)
- [Official Gleam Discord Server](https://discord.gg/gleam-lang)

With these resources, you can further explore the capabilities of Gleam and create amazing projects. Happy coding!