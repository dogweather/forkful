---
aliases:
- /en/elixir/starting-a-new-project/
date: 2024-01-20 18:03:06.150515-07:00
description: "Starting a new Elixir project means setting up a fresh Mix project structure.\
  \ Programmers do this to kick things off with organized code and automated\u2026"
lastmod: 2024-02-18 23:09:10.766383
model: gpt-4-1106-preview
summary: "Starting a new Elixir project means setting up a fresh Mix project structure.\
  \ Programmers do this to kick things off with organized code and automated\u2026"
title: Starting a new project
---

{{< edit_this_page >}}

## What & Why?
Starting a new Elixir project means setting up a fresh Mix project structure. Programmers do this to kick things off with organized code and automated tooling, helping to build and test their Elixir applications efficiently.

## How to:
To create a new project, use the `mix new` command:

```elixir
$ mix new my_app
```

You'll see something like this:

```
* creating README.md
* creating .formatter.exs
* creating .gitignore
* creating mix.exs
* creating lib
* creating lib/my_app.ex
* creating test
* creating test/test_helper.exs
* creating test/my_app_test.exs
```

Head into your new project directory:

```elixir
$ cd my_app
```

Now, you can run your project or its tests:

Run your project:

```elixir
$ iex -S mix
```
Test it:

```elixir
$ mix test
```

## Deep Dive
Elixir's build tool, Mix, came from a desire to provide a reliable and unified way to create, configure, and manage projects. It's influenced by tools from other ecosystems, like Ruby's Bundler and Rake. Mix brings dependency management and task automation into Elixir's toolbelt. Its alternatives in other languages might be npm for Node.js or Maven for Java. However, Mix is tailored to the Elixir runtime and integrated with its idiomatic patterns. The `mix new` command scaffolds a conventional structure with predefined directories and files, such as configuration files, module definitions, and test suites. Following conventions is key in Elixir; it encourages code consistency and readability across Elixir projects.

## See Also
- Official `mix` documentation: [https://hexdocs.pm/mix/Mix.html](https://hexdocs.pm/mix/Mix.html)
- Elixir School's project guide: [https://elixirschool.com/en/lessons/basics/mix/](https://elixirschool.com/en/lessons/basics/mix/)
