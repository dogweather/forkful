---
title:                "Printing debug output"
html_title:           "Elixir recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why

Debugging is an essential part of any programming language and printing debug output is an effective way to identify and resolve errors or issues in your code. It allows developers to see the values of variables and the flow of their program, making it easier to spot any mistakes or unexpected behavior.

## How To

To print debug output in Elixir, you can use the `IO.inspect` function. Simply pass in the variable or expression you want to see as a parameter and it will output the value or result to your console. Let's take a look at an example:

```Elixir
name = "John"
IO.inspect(name)
```

The output of the above code will be `John` in your console. In addition to basic values like strings and integers, `IO.inspect` can also handle more complex structures such as lists, tuples, and maps. Let's see how it works with a list:

```Elixir
names = ["John", "Jane", "Bob"]
IO.inspect(names)
```

The output will be `[John, Jane, Bob]` which is the list of names we defined. You can also pass in multiple arguments to `IO.inspect`, making it easier to print several values in one go. For example:

```Elixir
IO.inspect(name, names)
```

This will output both `John` and `[John, Jane, Bob]` to your console. `IO.inspect` is a great tool for quickly debugging your code and understanding the values of your variables.

## Deep Dive

`IO.inspect` is a wrapper for the `IO.inspect/2` function, which takes in two parameters: the value to be inspected and a keyword list of options. These options allow you to customize the output of `IO.inspect` to suit your needs. Some commonly used options are `:label` to provide a label for the output and `:pretty` to format the output in a more readable way. Let's take a look at an example using these options:

```Elixir
name = "Jane"
IO.inspect(name, label: "Name", pretty: true)
```

The output of this code will be:

```
Name: "Jane"
```

which is much easier to read than just `Jane`. These options can be very helpful when dealing with more complex data structures or when you want to add more context to your debugging output. For a full list of available options, check out the [official documentation](https://hexdocs.pm/elixir/IO.html#inspect/2).

## See Also
- [Debugging in Elixir using IO.inspect](https://elixir-lang.org/getting-started/debugging.html#io.inspect)
- [Elixir School: IO module](https://elixirschool.com/en/lessons/basics/io/#io-module)
- [Code snippets using IO.inspect](https://gist.github.com/mattforni/50bfb8fbbbd21794fc9e)