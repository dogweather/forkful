---
title:    "Elixir recipe: Reading command line arguments"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why

If you're new to Elixir programming, you may be wondering why it's important to learn about reading command line arguments. Command line arguments allow you to pass information to your Elixir programs when executing them from the command line. This can be useful for creating interactive programs or for automating tasks with advanced options.

## How To

In Elixir, command line arguments are accessed through the `System.argv` function. Let's take a look at an example where we want to print out the value of the first argument passed in:

```Elixir
# get_command_line_arg.exs

arg = System.argv |> Enum.at(1)
IO.puts("Argument: #{arg}")
```
```
$ elixir get_command_line_arg.exs Hello
Argument: Hello
```

In the example, we use the `Enum.at/2` function to retrieve the first argument from the `System.argv` list and then print it out using `IO.puts/1`.

If you want to access multiple arguments, you can use the `Enum.slice/2` function to return a sublist of arguments starting from the specified index. Let's see how this works in practice:

```Elixir
# get_multiple_args.exs

args = System.argv |> Enum.slice(1, 3)
IO.inspect(args)
```
```
$ elixir get_multiple_args.exs Hello World 123
["Hello", "World", "123"]
```

In this example, we use `Enum.slice/2` to return a list of 3 arguments starting from the second index (1). We then use `IO.inspect/1` to print out the list for inspection.

## Deep Dive

In Elixir, command line arguments are represented as a list of strings. This means that if you want to pass in numbers or other data types, you will need to convert them using the appropriate functions, such as `String.to_integer/1` or `String.to_float/1`. Additionally, you can use the `OptionParser` module to handle more complex argument parsing and validation.

It's important to note that command line arguments are not limited to just strings and can also include special characters and options, such as flags (-x), switches (--verbose), and arguments with values (-f filename). You can access these different types of arguments by using the `OptionParser.parse/2` function.

## See Also

- Elixir `System` module - https://hexdocs.pm/elixir/System.html
- Elixir `Enum` module - https://hexdocs.pm/elixir/Enum.html
- Elixir `OptionParser` module - https://hexdocs.pm/elixir/OptionParser.html