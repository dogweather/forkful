---
title:                "Починаємо новий проект"
date:                  2024-01-20T18:03:36.971791-07:00
model:                 gpt-4-1106-preview
simple_title:         "Починаємо новий проект"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why?
Що таке новий проект і навіщо програмісти його створюють?

Starting a project means setting up the workspace where your new code will live. Programmers start new projects to solve problems, explore ideas, or learn something new.

## How to:
Як це зробити:

```gleam
// Assuming you have Gleam installed
// Create a new project with `gleam new`
gleam new my_cool_project

// Navigate to your new project directory
cd my_cool_project

// Build your project
gleam build
```

Output:

```plaintext
Your Gleam project my_cool_project has been successfully created.
The `src` directory is where your .gleam files go.
After writing some code, run `gleam build` to compile.
```

## Deep Dive
Що потрібно знати детальніше:

In the past, Gleam borrowed much from Erlang and aimed to provide a safer, more user-friendly alternative. Now, it's a standalone language with a growing ecosystem. Starting a new project in Gleam gives you a solid foundation with the reliability of the Erlang virtual machine (BEAM) and a modern type system.

Alternatives like Elixir or Erlang offer different experiences but target the same VM. Gleam stands out with its strong type system and compile-time safety, while Elixir shines with great documentation and a more mature ecosystem.

When you initialize a project in Gleam, the tooling generates necessary files and directories, setting up a conventional structure. This usually means having `src`, `test`, and `gen` directories, a `gleam.toml` configuration file, and other boilerplates, giving you a predictable development environment.

## See Also
Що ще може бути корисним:

- The official Gleam website for documentation: [https://gleam.run](https://gleam.run)
- An introduction to the Gleam language for more context: [https://gleam.run/book](https://gleam.run/book)
- The GitHub repository for Gleam for contributing or issues: [https://github.com/gleam-lang/gleam](https://github.com/gleam-lang/gleam)
- An awesome list of Gleam resources: [https://github.com/gleam-lang/awesome-gleam](https://github.com/gleam-lang/awesome-gleam)
