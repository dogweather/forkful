---
title:                "Starting a new project"
html_title:           "Elixir recipe: Starting a new project"
simple_title:         "Starting a new project"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why?

Starting a new project in Elixir is the process of creating a new code base for a program or application. Programmers often start new projects to develop new features, improve existing ones, or explore new ideas.

## How to:

To start a new project in Elixir, we will use the `mix` command. 

```Elixir
mix new my_project
```

This creates a new project folder with the name `my_project` and sets up the necessary files and directories for an Elixir project. 

We can also specify the `--sup` flag to create a project with a `supervisor` module, which will supervise the application's processes. 

```Elixir
mix new --sup my_project
```

The `--sup` flag is useful for projects that require more robust error handling and process management. 

## Deep Dive:

Elixir's `mix` command is inspired by the Ruby on Rails `rails` command, which also creates a new project structure and sets up commonly used files and directories. However, Elixir's `mix` is more flexible and allows for customization by using different mixins. 

An alternative to using `mix` is manually creating the necessary files and folders, but this can be time-consuming and error-prone. Therefore, it is recommended to use `mix` to start new projects.

Under the hood, the `mix` command uses a template engine called `mix new`. This allows for the easy creation of new projects with customizable options. 

## See Also:

Official Elixir documentation on `mix new`: https://hexdocs.pm/mix/Mix.Tasks.New.html