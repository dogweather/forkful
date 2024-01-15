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

## Why

If you're a developer looking to start a new project, you may want to consider Elixir. This functional programming language is known for its scalability, fault tolerance, and performance, making it a popular choice for building robust and reliable applications.

## How To

To get started with Elixir, the first step is to install the latest version of the language. You can do this by following the official installation instructions on the Elixir website.

Once Elixir is installed, you can create a new project by using the `mix` tool which comes bundled with Elixir. Simply navigate to the desired directory and run `mix new project_name` in your terminal. This will generate a new project with the necessary boilerplate code.

Let's take a look at a simple example of how to use Elixir. We'll create a function that takes in a name as an argument and returns a personalized greeting:

```elixir
def greeting(name) do
  "Hello #{name}, welcome to the world of Elixir!"
end

greeting("Sam")
# Output: Hello Sam, welcome to the world of Elixir!
```

As you can see, Elixir uses a functional style of programming, where functions are its building blocks. It also supports pattern matching and pipe operators, making code easier to read and understand.

## Deep Dive

One of the key features of Elixir is its use of the Erlang Virtual Machine (VM). This allows Elixir to leverage the robustness and scalability of Erlang, while also providing a more modern and user-friendly syntax.

When starting a new project in Elixir, it's important to consider the OTP (Open Telecom Platform) framework. OTP provides a set of abstractions and libraries that enable developers to build fault-tolerant and distributed systems. It also includes features such as supervision trees and gen servers, which are crucial for creating resilient applications.

Moreover, Elixir comes with a built-in tool called "Mix" that can aid in managing dependencies, running tests, and generating documentation for your project. It also has a vibrant community that continuously contributes new libraries and tools, making it easier for developers to build complex systems.

See Also
- [Official Elixir Website](https://elixir-lang.org/)
- [Elixir School](https://elixirschool.com/)
- [Awesome Elixir](https://github.com/h4cc/awesome-elixir)