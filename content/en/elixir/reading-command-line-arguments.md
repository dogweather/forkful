---
title:                "Elixir recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why

When it comes to programming in Elixir, there are many tools available to make our lives easier. One of these tools is reading command line arguments. Understanding how to do this can save you time and make your code more efficient. In this post, we will explore how to read command line arguments in Elixir.

## How To

To read command line arguments in Elixir, we can use the `System.argv` function. This function returns a list of command line arguments passed to the program. Let's take a look at a simple example:

```
Elixir
defmodule CommandLine do
  def main do
    args = System.argv
    IO.puts "Arguments passed: #{inspect args}"
  end
end

CommandLine.main()
```

In this example, we define a module `CommandLine` and a function `main`. Inside the `main` function, we use `System.argv` to retrieve the command line arguments and then use `IO.puts` to output them on the screen. Let's say we run this program with the following command: `elixir command_line.exs argument1 argument2`. The output will be:

```
Arguments passed: ["argument1", "argument2"]
```

We can also access specific arguments by using their position in the list. For example, if we want to access the second argument, we can use `args[1]`. Keep in mind that the first argument will always be the name of the script itself.

We can also use `System.argv` inside a `mix.exs` file to pass arguments to our project when running tasks. This can be useful for different configurations or settings.

## Deep Dive

Behind the scenes, `System.argv` is using the Erlang `os:getarg` function to retrieve the arguments from the command line. This function uses the operating system's API to access the arguments. It's important to note that command line arguments are always passed as strings, so we may need to convert them to the desired data type before using them in our code.

Another helpful tool for working with command line arguments is the `OptionParser` module. This module provides a convenient way to parse and validate options passed through the command line. It also allows us to define default values for options if they are not provided.

## See Also

To learn more about reading command line arguments in Elixir, check out these resources:

- [Elixir Documentation on System.argv](https://hexdocs.pm/elixir/System.html#argv/0)
- [Elixir Documentation on OptionParser](https://hexdocs.pm/elixir/OptionParser.html)
- [Blog post on Command Line Arguments in Elixir by PragTob](https://pragtob.wordpress.com/2013/10/22/bite-sized-command-line-applications-with-elixir-part-2/)