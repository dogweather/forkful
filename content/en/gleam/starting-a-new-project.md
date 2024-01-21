---
title:                "Starting a new project"
date:                  2024-01-20T18:03:21.460852-07:00
model:                 gpt-4-1106-preview
simple_title:         "Starting a new project"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why?

Starting a new project means initializing a fresh codebase with the necessary structure and configuration. Programmers do it to kickstart development with a clean slate, ensuring best practices and organization from the get-go.

## How to:

To create a new Gleam project, you’ll need the `gleam` command-line tool. Install it, and then run:

```shell
gleam new my_cool_project
```

This creates a new directory named `my_cool_project` with a basic project structure:

```plaintext
my_cool_project/
├── gleam.toml
├── src
│   └── my_cool_project.gleam
└── test
    └── my_cool_project_test.gleam
```

The `src` directory contains the main Gleam module, and the `test` directory is where your tests will live. Let’s peek at the default `my_cool_project.gleam`:

```gleam
pub fn hello_world() {
  "Hello, world!"
}
```

Sweet and simple. Now you've started a new Gleam project!

## Deep Dive

Gleam burst onto the scene around 2018, aiming to bring strong static typing to the Erlang ecosystem—without losing its famed reliability and concurrency model.

Alternatives to starting a project with `gleam new` might involve cloning a template from a repository or manually creating the file structure. However, using the Gleam tool provides a consistent starting point, and it's designed to work seamlessly within the ecosystem.

Behind the scenes, `gleam new` sets up a `rebar3` or `mix` project depending on your preference (the default is `rebar3`). It populates the necessary configuration files such as `gleam.toml` for dependency management and project settings, and `rebar.config` or `mix.exs` for Erlang or Elixir interoperability, respectively.

## See Also

- Gleam's official getting started guide: [https://gleam.run/book/getting-started/](https://gleam.run/book/getting-started/)
- GitHub repository of Gleam: [https://github.com/gleam-lang/gleam](https://github.com/gleam-lang/gleam)
- Gleam's standard library documentation: [https://hexdocs.pm/gleam_stdlib/](https://hexdocs.pm/gleam_stdlib/)