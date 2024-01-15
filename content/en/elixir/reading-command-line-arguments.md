---
title:                "Reading command line arguments"
html_title:           "Elixir recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why

Reading command line arguments is a fundamental skill in programming that allows you to interact with your code in a more dynamic and customizable way. It also allows you to pass information and parameters to your code without hardcoding them, making your code more flexible and efficient.

## How To

To read command line arguments in Elixir, we will use the `System.argv` function. This function returns a list of all the arguments passed to our script when it was executed. Let's see it in action:

```Elixir
defmodule CommandLine do
  def run(args) do
    IO.inspect args
  end
end

CommandLine.run(System.argv)
```

Save this code in a file called `command_line.exs` and run it with `elixir command_line.exs hello world`. You should see the output `["hello", "world"]` printed in your console. You can pass as many arguments as you want, and they will all be captured in the `args` variable as a list.

We can also use pattern matching to access individual arguments. For example, if we want to assign the first argument to a variable called `greeting` and the second argument to a variable called `name`, we can do it like this:

```Elixir
defmodule CommandLine do
  def run([greeting, name | _rest]) do
    IO.puts "#{greeting} #{name}!"
  end
end

CommandLine.run(System.argv)
```

Now when we run our script with `elixir command_line.exs "Hello" "John"`, it will output `Hello John!` in the console.

## Deep Dive

The `System.argv` function is a part of the `System` module, which is a collection of functions that provides access to system-level operations. It also has a counterpart, `System.get_env`, that allows us to retrieve environment variables and use them in our code.

We can also use the `System.argv` function in a mix project. In this case, the arguments will be passed to the `mix run` command, and the first argument will always be the path to the script being executed.

## See Also

- [Elixir Documentation on System](https://hexdocs.pm/elixir/System.html)
- [Elixir Scripting](https://elixir-lang.org/getting-started/introduction.html#scripting)