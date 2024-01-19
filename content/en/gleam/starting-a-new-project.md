---
title:                "Starting a new project"
html_title:           "Bash recipe: Starting a new project"
simple_title:         "Starting a new project"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why?

Starting a new project is the creation of a fresh idea on a blank canvas; it's where innovation births. Coders kickstart new projects to materialize designs, solve problems, create applications, or just to explore the depths of a new technology.

## How to:

In Gleam, creating a new project is a breeze, using predefined templates, elements, and the `gleam new` command.

```Gleam
$ gleam new my_cool_project
```

If you're looking for a back-end application: 

```Gleam
$ gleam new my_cool_project --template gleam/http
```

Running these commands sets up a standard, ready-to-go project architecture in a directory called "my_cool_project". 

## Deep Dive 

Gleam stems from the Erlang ecosystem's roots. Its 'new project' generation utilizes Rebar3 (Erlang's build tool), yielding cross-compatibility with Erlang and Elixir.

Alternatively, you can also start projects manually, crafting each file and folder from scratch, but it's not recommended because of the efficiency and convenience provided by the built-in project generator.

Once a project is created, it will follow a pre-defined structure:

```Gleam
src/                  # Source Gleam files
test/                 # Test Gleam files
rebar.config          # Rebar3 config file
gleam.toml            # Gleam configuration file
.gitignore            # Whitelist for version control
```

## See Also

- [Gleam's Official Tutorial](https://gleam.run/tour/)
- [Project Setup in Gleam](https://gleam.run/guides/creating-our-first-project/)
- [Rebar3 Docs](https://www.rebar3.org/docs/getting-started)
- [Erlang Language Basics](http://erlang.org/doc/getting_started/users_guide.html)