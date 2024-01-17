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

## What & Why?

Reading command line arguments is the process of obtaining inputs from the user through the command line interface. Programmers do this in order to make their programs more dynamic and configurable, as it allows for user input to be passed in when the program is executed.

## How to:

```Elixir
# To read command line arguments, we first need to use the `System.argv/0` function from the `System` module.
# It returns a list of strings, where each element represents an argument passed in by the user.

args = System.argv

# To access specific arguments, we can use list indexing as follows:

first_arg = args[0]
second_arg = args[1]

# We can also use `length/1` function from the `Enum` module to get the total number of arguments passed in.

args_length = Enum.length(args)

# Let's try it out with a simple program that greets the user based on their input:

defmodule Hello do
  def main do
    args = System.argv
    name = args[0]
    IO.puts("Hello #{name}, welcome to Elixir!")
  end
end

# Let's compile and run this program with the following command:

$ elixirc hello.ex
$ elixir Hello world

# Output:
Hello world, welcome to Elixir!
```

## Deep Dive:

Reading command line arguments has been a standard feature in most programming languages for a long time. It allows for command line interfaces to be interactive and provide a more user-friendly experience. Other alternatives to reading command line inputs include using environment variables or configuration files.

Internally, the `System.argv/0` function makes a call to the `:init.get_argument/2` from the Erlang runtime system. This function is responsible for getting all the arguments passed in during program execution. Additionally, Elixir also provides the `OptionParser` module for more robust and structured argument parsing.

## See Also:

- [`System` module documentation](https://hexdocs.pm/elixir/System.html)
- [`OptionParser` module documentation](https://hexdocs.pm/elixir/OptionParser.html)
- [Erlang `:init` module documentation](http://erlang.org/doc/man/init.html)