---
title:                "Gleam recipe: Starting a new project"
simple_title:         "Starting a new project"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Why
Starting a new programming project can be both exciting and overwhelming. But with the rise of new programming languages like Gleam, it's the perfect time to dive into something new and unleash your creative potential. Whether you're a seasoned developer or a curious beginner, learning Gleam can open up a world of possibilities for your next project. 

## How To
To get started with Gleam, you'll first need to have the Gleam compiler and a code editor of your choice installed on your system. Once that's done, you can start creating your first Gleam project. To create a new project, simply open your terminal and type in the following command:

```
Gleam new [project_name]
```
This will generate a new project directory with all the necessary files and folders to get you started. You can then navigate to your project directory and open it in your code editor.

Next, you'll need to create a new module, which is a group of related functions and types. Modules help organize your code and make it more readable. To create a new module, use the following command:

```
Gleam new module [module_name]
```

Within each module, you can define functions and types. Here's an example of a simple function that adds two numbers together:

```
Gleam def add(x, y) {
    x + y
}
```

You can also define custom types in Gleam. Here's an example of a custom type `Person` with two fields `name` and `age`:

```
Gleam type Person {
    name: String,
    age: Int
}
```

Once you've written your code, you can compile it by running the following command in your terminal:

```
Gleam build
```

This will generate a compiled version of your code, which you can then run using the following command:

```
Gleam run
```

## Deep Dive
Starting a new project in Gleam also means understanding its unique features, such as pattern matching, immutability, and its powerful type system. These features not only make your code more concise and readable but also provide more safety and predictability in your code. Additionally, Gleam integrates seamlessly with other languages like Erlang and Elixir, allowing you to tap into their powerful ecosystems.

To truly take advantage of Gleam's capabilities, it's important to familiarize yourself with its syntax and concepts. You can find comprehensive documentation, tutorials, and code examples on the official Gleam website. Joining the Gleam community and engaging in discussions with other users can also help you deepen your understanding of the language.

## See Also
- Official Gleam documentation: [https://gleam.run](https://gleam.run)
- Gleam GitHub repository: [https://github.com/gleam-lang/gleam](https://github.com/gleam-lang/gleam)
- Gleam community forum: [https://elixirforum.com/c/languages/gleam](https://elixirforum.com/c/languages/gleam)