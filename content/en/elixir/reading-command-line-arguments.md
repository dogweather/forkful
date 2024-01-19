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

Reading command line arguments in Elixir is about dissecting and utilizing inputs given to a script during execution. This practice is integral in molding customizable, interactive, and more dynamic programs.

## How to:

Elixir uses the `System.argv/0` function which obtains the list of arguments passed. Let's illustrate this concept with an example:

```elixir
IO.inspect(System.argv())
```

For instance, if we run `elixir test.exs arg1 arg2`, the output will be:

```elixir
["arg1", "arg2"]
```
Our script "test.exs" received the arguments "arg1" and "arg2" that we can use for configurable processing within the script.

## Deep Dive

Historically, command-line argument handling was prominent in scripting languages or C-style syntax languages. Elixir carries this tradition forward, seamlessly integrating it with its functional paradigm.

There are alternatives to reading command-line arguments in Elixir, including option parsers like `OptionParser.parse/2` which can deliver richer data structures and more complex handling for arguments.

The implementation of `System.argv/0` fetches its data from a BEAM (Erlang Virtual Machine) call, that compiles and executes Elixir. It's worth mentioning, the Elixir scripts are compiled before running. Hence, arguments can't be changed dynamically while the script is running.

## See Also:

To dig deeper, visit these handy links:

- Elixir's official doc for System.argv/0: https://hexdocs.pm/elixir/System.html#argv/0
- Info on the OptionParser: https://hexdocs.pm/elixir/OptionParser.html
- More on command-line arguments in programming: https://en.wikipedia.org/wiki/Command-line_interface#Arguments