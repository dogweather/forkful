---
title:                "Deleting characters matching a pattern"
html_title:           "Elixir recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why?

Deleting characters matching a pattern allows manipulation of strings by removing certain specified recurring features. Software developers do this to boost efficiency, improve code readability, and cater to specific data requirements.

## How To:

For basic string manipulations in Elixir, we use the `String.replace/3` function. Ruthlessly practical code below:

```Elixir
iex> String.replace("Programming is fun!", " ", "")
"Programmingisfun!"

iex> String.replace("123abc123", "123", "")
"abc"
```

In these examples, we're taking out all spaces in the first, and in the second, we're removing patterns '123'.

Now, if the pattern to be replaced is complex, we use 'regex'. Get a load of this:

```Elixir
iex> Regex.replace(~r/\d/, "Elixir1234", "")
"Elixir"
```

In this case, we're removing all numerical characters (denoted by `\d`) from "Elixir1234".

## Deep Dive:

Historically, Elixir's native functionalities derive from Erlang. Pattern matching was (and remains) a core feature for readability and simplification.

Alternatives? Shell out extra time and energy with building custom functions if regex doesn't cut it for you. There's also the `tr` Unix command for character removal.

Under the hood, Elixir transforms your code into Abstract Syntax Tree (AST) format, then compiles into BEAM bytecode. Elixir's `String.replace/3` and `Regex.replace/3` methods handle the grunt work of pattern removal.

## See Also:

For all your Elixir textual needs, check out the official HexDocs:

- Elixir Strings: https://hexdocs.pm/elixir/String.html
- Elixir Regex: https://hexdocs.pm/elixir/Regex.html

And of course, Erlang's underlying string package documentation:

- Erlang Documentation: http://erlang.org/doc/man/string.html

Happy coding!