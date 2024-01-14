---
title:                "Elixir recipe: Starting a new project"
simple_title:         "Starting a new project"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Why

When starting a new project, it's important to choose the right programming language. Elixir, a functional programming language built on top of the Erlang VM, offers a unique set of features that make it an ideal choice for web development, networking, and distributed systems. In this blog post, we'll explore why Elixir is a great choice for your next project.

## How To

To get started with Elixir, you'll need to have Elixir and the Erlang VM installed on your machine. Once you have that set up, you can create a new project by running the following command in your terminal:

```
mix new my_project
```

This will create a new project folder with all the necessary files and configurations. Let's take a closer look at some of these files and how they are used in an Elixir project.

### The Mix file

The Mix file is where you can define your project dependencies, tasks, and other configurations. Let's say we want to add the `httpoison` library to our project. We can do so by adding the following line to the `deps` function in the Mix file:

```
{:httpoison, "~> 1.6"}
```

After saving the file, we can run `mix deps.get` in our terminal to download and install the new dependency. This allows us to use functions from the `httpoison` library in our project.

### The Lib folder

The Lib folder is where you will write most of your Elixir code. Let's say we want to create a function to make a GET request using `httpoison`. We can do so by creating a new file in the Lib folder and adding the following code:

```
def make_get_request(url) do
  HTTPoison.get(url)
end
```

We can then use this function in other files in our project. For example, let's say we want to make a GET request to `https://example.com` in our `my_project.ex` file. We can do so by calling our function with the desired URL:

```
result = make_get_request("https://example.com")
```

### Running the project

To run our project, we can use `mix run` in our terminal. This will start our project and run any code in our `main` function. Alternatively, we can use `iex -S mix` to start an interactive Elixir shell and run our project from there.

## Deep Dive

One of the reasons Elixir is a great choice for projects is because of its focus on concurrency and fault-tolerance. Elixir uses lightweight processes, also known as "actors", to handle parallel computations. These processes communicate with each other through message passing, allowing for a highly scalable and fault-tolerant system.

Elixir also has a robust set of tools for building distributed systems. The `mix` tool, which we used earlier, provides tasks for creating and managing clusters of Elixir nodes. This allows you to distribute your workload across multiple nodes, making it easier to scale your application as needed.

## See Also

- Official Elixir website: https://elixir-lang.org/
- Elixir documentation: https://hexdocs.pm/elixir/
- Elixir Forum: https://elixirforum.com/