---
title:                "Elixir recipe: Starting a new project"
programming_language: "Elixir"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Why Start a New Project in Elixir?

Starting a new project in Elixir can be a great choice for many developers. Elixir is a functional, dynamic and highly scalable language, making it a powerful tool for building applications of all sizes. It also has a robust and growing community, providing a wealth of resources and support for developers. So if you're looking to build a robust and efficient application, Elixir may be just the right choice for you.

# How To Start a New Project in Elixir

To start a new project in Elixir, you'll need to have Elixir and its build tool, Mix, installed on your system. Once they are installed, follow these simple steps:

1. Create a new project directory: `mix new my_project`

2. Change into the project directory: `cd my_project`

3. Generate an Elixir script: `mix new --app my_app`

4. Navigate to the app directory: `cd apps/my_app`

5. Run the test suite: `mix test`

6. Build the application: `mix compile`

7. Run the application: `mix run --no-halt`

These steps will get you started with a basic Elixir project. You can then proceed to add more functionality and build on top of this initial structure.

```Elixir
# Basic Elixir script
defmodule MyApp do
  def hello do
    IO.puts "Hello world!"
  end
end

MyApp.hello()

# Output: Hello world!
```

# Deep Dive into Starting a New Project in Elixir

When starting a new project in Elixir, it's important to understand the principles of functional programming. This means writing code that is immutable, meaning that variables cannot be changed after they are assigned. This may seem restrictive, but it actually promotes better and more reliable code.

It's also worth noting that Elixir has a built-in package manager, Hex, which allows for easy installation and management of dependencies. This allows for a more streamlined and efficient development process.

Another key feature of Elixir is its ability to run multiple processes and manage them using the actor model. This makes Elixir ideal for building highly concurrent and distributed applications.

# See Also

Here are some useful resources for learning more about starting a new project in Elixir:

- [Elixir official website](https://elixir-lang.org/)
- [Elixir School](https://elixirschool.com/)
- [Elixir Forum](https://elixirforum.com/)
- [Awesome Elixir](https://github.com/h4cc/awesome-elixir)

Start your Elixir journey today and see the amazing things you can build with this powerful language. Happy coding!