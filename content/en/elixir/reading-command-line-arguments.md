---
title:                "Reading command line arguments"
html_title:           "C++ recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Working with Command Line Arguments in Elixir

## What & Why?

Command line arguments are parameters used to modify a program's behavior during its execution. They provide a flexible way for users to interact with our program, enabling bespoke execution paths and custom user input.

## How to:

Use Elixir's `System.argv/0` function to access command line arguments. Provide arguments at runtime after the script name. Here's a simple illustration:

```elixir
# commandline.exs

defmodule CommandLine do

  def main(args) do
    IO.inspect(args)
  end

end

CommandLine.main(System.argv())
```

Execute with arguments:

```bash
elixir commandline.exs arg1 arg2 arg3
```

This will print:

```bash
["arg1", "arg2", "arg3"]
```
Showing that the arguments are passed as a list of strings to your script.

## Deep Dive

Historically, command line arguments have been a staple in Unix-like operating systems, powering versatile command-line interfaces. Elixir, building on this heritage, uses Erlang's `init:get_plain_arguments/0` function under the hood.

An alternative to command line arguments is using environment variables with `System.get_env/0`. However, this proves less flexible as these are mainly static.

When you call `System.argv/0`, Elixir fetches arguments passed to the Erlang runtime. Any arguments before `--` are consumed by the runtime itself, while the rest are passed to your script. 

Elixir also supports argument parsing with `OptionParser.parse/2`, enabling more advanced argument structures like flags or switches.

## See Also

1. Understanding Command Line Arguments: https://en.wikipedia.org/wiki/Command-line_argument_parsing
2. Elixir's `System.argv/0` : https://hexdocs.pm/elixir/System.html#argv/0
3. OptionParser in Elixir: https://hexdocs.pm/elixir/OptionParser.html
4. Erlang's command line handling: http://erlang.org/doc/man/init.html
5. UNIX commands and arguments: https://www.ibm.com/docs/en/aix/7.1?topic=concepts-command-line-arguments