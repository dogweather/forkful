---
title:                "Starting a new project"
html_title:           "Bash recipe: Starting a new project"
simple_title:         "Starting a new project"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Kickstart Using Elixir: Starting A New Project 

## What & Why?
Starting a new project is creating a fresh coding environment where you scaffold your codebase structure. Programmers do this to maintain a well-structured, clean, and maintainable project.

## How to:
Starting a new project in Elixir is straightforward, thanks to Mix, Elixir's build tool. Here is how you do it:

```Elixir
$ mix new hello_world
```

This command creates a new directory called `hello_world` with all necessary project files. Check what's inside:

```Elixir
$ cd hello_world
$ ls
```

Output:

```
_config     lib        test     mix.exs
```

Your new Elixir project is now ready for development. The `lib` directory will contain your source code and the `test` your unit tests.

## Deep Dive
Elixir introduced Mix looking back at Erlang's build tools shortcomings. It delivers a user-friendlier command-line interface, managing dependencies, running tests, and much more. It's a powerful tool that simplifies setting up and managing an Elixir project.

While alternatives to Mix exist, such as Rebar3 (a well-known build tool in the Erlang world), they aren't as tightly integrated with Elixir. Selecting one over the other will depend largely on your project's specifics and your preference.

When you run `mix new`, under the hood, Mix creates a basic directory structure and a few vital files like `mix.exs`. This file is where you define your application, dependencies, and project version, becoming the go-to place for managing anything related to your project.

## See Also
1. [Elixir's Official Documentation on Mix](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html)
2. [Introduction to Mix on Elixir School](https://elixirschool.com/en/lessons/basics/mix/)