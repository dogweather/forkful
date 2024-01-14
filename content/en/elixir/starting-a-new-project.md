---
title:    "Elixir recipe: Starting a new project"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/elixir/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Why

Starting a new programming project can seem like a daunting task, but with Elixir, it can be a breeze. This functional programming language offers features such as concurrency and fault tolerance, making it a powerful choice for building robust and scalable applications.

## How To

To create a new Elixir project, you will first need to have Elixir installed on your computer. Once that is done, you can follow these simple steps:

1. Open your terminal and navigate to the directory where you want to create your project.
2. Type `mix new [project name]` and press enter. This will create a new project with the name you specified.
3. Navigate into the project directory using `cd [project name]`.
4. You will see a few files and folders already created for you, such as `lib` for your source code and `test` for your tests.
5. To run your project, use the command `mix run -e [module_name]`, where `[module_name]` is the name of the module containing your `main` function.
6. You can also use `mix test` to run your tests.

Let's try a simple example. Create a new project called "hello_world" using `mix new hello_world`. Then open the `lib` folder and create a new file named `greeting.ex`. Paste the following code inside the file:

```elixir
defmodule Greeting do
  def hello do
    IO.puts "Hello world!"
  end
end
```

Save the file and then go back to your terminal. Run the command `mix run -e Greeting.hello` and you should see "Hello world!" printed on the screen.

## Deep Dive

Now that you have created a new project and run your first Elixir code, let's take a deeper look at what's going on behind the scenes. When you run `mix new [project name]`, Elixir will generate a `mix.exs` file. This is an Elixir script that defines your project and its dependencies.

Inside the `lib` folder, you will also find a `hello_world.ex` file. This is your application's entry point, where you can define your `main` function and add any other code you need to run your project.

Elixir also comes with a built-in testing framework called ExUnit. The `test` folder contains a file named `hello_world_test.exs` where you can write tests for your code.

## See Also

- [Official Elixir website](https://elixir-lang.org/)
- [Elixir tutorials on ElixirSchool](https://elixirschool.com/)
- [Elixir forums on ElixirForum](https://elixirforum.com/)