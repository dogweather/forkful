---
title:    "Elixir recipe: Reading command line arguments"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Why

The ability to read and process command line arguments is an important skill for any programmer, particularly for those working with Elixir. Command line arguments allow for flexible and dynamic control over a program's behavior and can greatly enhance its functionality.

## How To

Reading command line arguments in Elixir is a straightforward process. First, we need to access the command line arguments using the `System.argv` function, which returns a list of strings. From there, we can use pattern matching to extract specific arguments and their values.

Let's take a look at an example:

```
Elixir

defmodule CommandLine do
  def main(args) do
    case args do
      [_, option] -> IO.puts("Option: #{option}")
      [_, option, value] -> IO.puts("Option: #{option}, Value: #{value}")
    end
  end
end

CommandLine.main(System.argv)
```

In this example, we first match the list of arguments to see if it contains two or three items. The first item is ignored as it is always the name of the script being executed. If the list contains two items, we can assume that the second item is an option with no value. If it contains three items, the second item is the option and the third item is its value.

Let's try running this script with different arguments:

```
$ elixir command_line.ex -a
Option: -a

$ elixir command_line.ex -v 2
Option: -v, Value: 2
```

As you can see, our program is now able to handle multiple command line arguments and display their corresponding values.

## Deep Dive

While the above example is a simple implementation of reading command line arguments in Elixir, there are a few nuances to keep in mind. For example, Elixir also provides the `System.argv0` function which returns the full path of the script being executed, and the `System.argv/1` function which takes in a number and returns the argument at that position in the list.

Another important consideration is handling errors and edge cases. What happens if the user provides an invalid option or no arguments at all? It's important to think about these scenarios and have proper error handling in place to ensure a smooth and robust program.

Furthermore, Elixir also allows for the use of options with values in the form of `--option=value`. In this case, the list of arguments returned by `System.argv` will contain one item with the option and value concatenated together. This means that we would need to split the string and handle the option and value separately. It may be useful to use Elixir's `String.split/2` function for this task.

## See Also

- [Elixir Docs: System.argv/0](https://hexdocs.pm/elixir/System.html#argv/0)
- [Elixir Docs: String.split/2](https://hexdocs.pm/elixir/String.html#split/2)
- [Erlang Docs: os:getenv/1](https://erlang.org/doc/man/os.html#getenv-1)

Now that you know how to read command line arguments in Elixir, you can start incorporating them into your programs for a more dynamic and flexible experience. Happy coding!