---
title:    "Elixir recipe: Starting a new project"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Why

Are you looking to start a new programming project and want to explore a functional programming language? Look no further than Elixir! This powerful language, built on top of the Erlang virtual machine, offers a myriad of features and benefits that make it a great choice for both new and experienced developers.

## How To

To get started with a new Elixir project, first make sure you have the language installed on your system. You can easily do this by following the installation instructions on the official Elixir website.

Next, create a new project directory and navigate into it. In this directory, you can initialize a new Elixir project by running the following command:

```
mix new my_project
```

This will set up a basic project structure for you, complete with a `mix.exs` file for managing dependencies and configurations. From here, you can start writing your code in `.ex` files using your preferred text editor.

Let's take a look at a simple example of how to define and call a function in Elixir:

```
defmodule Calculator do
  def add(a, b) do
    a + b
  end
end

IO.puts(Calculator.add(2, 3))
```

In this example, we define a new module called `Calculator` and a function within it called `add`, which takes two parameters and returns their sum. We then use `IO.puts` to print out the result of calling this function with the values 2 and 3. Running this code will output `5` to the console.

But that's just scratching the surface of what Elixir can do. There are many more features and concepts to explore, such as pattern matching, concurrency with processes and OTP, and the powerful Phoenix web framework.

## Deep Dive

Now that you have a basic understanding of how to get started with an Elixir project, let's dive deeper into some key aspects of the language.

One of the great things about Elixir is its robust standard library, which includes modules for tasks like string manipulation, data structures, and networking. You can easily import these modules into your projects and take advantage of their functionality.

Another important aspect of Elixir is its emphasis on immutability and functional programming. This means that variables cannot be reassigned, and most functions do not have side effects. In addition, Elixir offers powerful tools for working with collections of data, such as the `Enum` and `Stream` modules.

Finally, it's worth mentioning the powerful tooling and community support available for Elixir. The `mix` build tool offers features like dependency management and code formatting, while the Elixir forums and Slack channels are great places to ask questions and learn from others.

## See Also

For more information on getting started with Elixir, check out these resources:

- Official Elixir website: https://elixir-lang.org/
- Elixir School: https://elixirschool.com/
- Elixir Forum: https://elixirforum.com/
- Elixir Slack: https://elixir-slackin.herokuapp.com/

Happy coding!