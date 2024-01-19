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

Starting a new project in programming is like creating a blueprint for a new idea. Programmers do it to plot out the structure and design of a software concept before jumping into coding. This way, they ensure systematic workflows and fewer bugs during the development phase.

## How to:

In Elixir, you start a new project using Mix, a build tool that ships with Elixir itself. Here's an example:

```elixir
mix new my_app
```

This command creates a new directory named `my_app` with the key files: `mix.exs`, `config`, `lib`, `test`, and a `.formatter.exs` file. The project's structure will look like this:

```elixir
my_app
├─ config
│  └─ config.exs
├─ lib
│  └─ my_app.ex
├─ test
│  ├─ my_app_test.exs
│  └─ test_helper.exs
├─ .formatter.exs
└─ mix.exs
```

To run your new project, use:

```elixir
iex -S mix
```

You're now in an Elixir shell with your project compiled and ready to go.

## Deep Dive

Today, starting a new project using Mix has become a standard in Elixir programming. But why Mix? Before its inception, Elixir developers were doing everything manually, which was error-prone and time-consuming. Mix simplified this by offering developers the ability to make reusable modules, thus making code more modular, efficient, and easy to manage.

While Mix is wonderful, it's not the only option for starting a new project. Other build tools like Rebar and CMake offer similar features, and the choice between them often boils down to the specifics of the project at hand and the developer's preference.

On the implementation front, Mix relies on the OTP (Open Telecom Platform) principles under the hood, which revolves around processes and message passing to create concurrent and scalable programs. 

## See Also

Elixir's official getting-started guide: https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html

"A Little Elixir & OTP" by Benjamin Tan: https://www.amazon.com/Little-Elixir-OTP-Benjamin-Tan/dp/1680502522

Elixir School on Mix: https://elixirschool.com/en/lessons/basics/mix/