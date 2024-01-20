---
title:                "Interpolating a string"
html_title:           "Arduino recipe: Interpolating a string"
simple_title:         "Interpolating a string"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

String interpolation in programming is a way to substitute single values or entire expressions into a string. Programmers do it to make their code cleaner and easier to understand.

## How to:

The Fish shell makes string interpolation straightforward- you use a variable directly within quotations. Check the code below:

```Fish Shell
set greeting "world"
echo "Hello, $greeting"
```

Here, `echo` will print `Hello, world`. The variable `$greeting` was interpolated into the string.

## Deep Dive

String interpolation has been around since the early programming languages, but has become a standard feature only in more recent ones. Fish shell, being a modern shell, takes the approach of Bash and other Unix-style shells but simplifies it. 

There are alternatives to string interpolation like string concatenation. For example:

```Fish Shell
set greeting "world"
echo "Hello, " + $greeting
```

This will also output `Hello, world`. However, it's not as readable and intuitive as string interpolation. 

In Fish shell, the interpolation works by replacing the variable with its value during the echo command. That's why enclosing the variable in quotes doesn't stop it from being replaced.

## See Also

For more details about string interpolation in Fish shell and other programming topics, check out these resources:

- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [String Interpolation on Wikipedia](https://en.wikipedia.org/wiki/String_interpolation)

Happy coding!