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

## What & Why?
Starting a new project is the process of initializing a new development environment for a specific idea or application. Programmers do it to create a separate workspace where they can build and manage the different components of their program in a structured manner.

## How to:
To kick off a Gleam (v0.13.2) project, first, you must install the Gleam compiler and rebar3 on your computer. Afterward, call `gleam new hello_world` in your terminal. This will generate your new project.

```Gleam
$ gleam new hello_world
* creating hello_world/README.md
* creating hello_world/.gitignore
* creating hello_world/rebar.config
* creating hello_world/.formatter.exs
* creating hello_world/lib/hello_world.gleam
Your Gleam project hello_world has been successfully created.
The rebar3 program needs to be in your PATH. See https://www.rebar3.org/ for installation instructions.
```

## Deep Dive
Starting a new project is a strategy that has its roots in the inception of program development. It allows developers to compartmentalize their work and manage complex applications effectively. The Gleam's approach derives from this principle. 

Alternatives include starting a minimal project with just a handful of files or simply running scripts without frameworks. However, Gleam's method offers a more organized structure suitable for complex or large-scale applications. 

Under the hood, `gleam new` command creates the necessary files and directory structures, providing a ready-to-use development environment. It also generates a .gitignore file for version control providers and a rebar.config file for project configuration.

## See Also
- The Gleam book: https://gleam.run/book/tour/hello-gleam.html
- Gleam GitHub Repository: https://github.com/gleam-lang/gleam
- Rebar3 Official Site: https://www.rebar3.org/