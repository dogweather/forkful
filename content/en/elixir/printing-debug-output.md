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

## What & Why?

Printing debug output is the process of displaying information about a program's operations and variables, which allows programmers to identify and fix issues in their code. It helps in understanding the flow of the program and identifying the cause of errors.

## How to:

To print debug output in Elixir, programmers can use the `IO.inspect/2` function. This function takes in two arguments: the variable or expression to be inspected, and an optional message to be displayed along with the output.

```Elixir
# Example 1: Inspecting a variable
name = "John"
IO.inspect(name)

# Output: "John"

# Example 2: Inspecting an expression with a message
num1 = 5
num2 = 10
IO.inspect(num1 + num2, "The sum is:")

# Output: The sum is: 15
```

## Deep Dive:

In the past, printing debug output was a common practice due to limited debugging tools. However, with the advancement of tools like debuggers and IDEs, it is now considered as a last resort for debugging and is mainly used for troubleshooting in production environments.

Alternative methods for debugging include using a debugger or logging statements. Logging allows for more fine-grained control over what is being outputted and is preferred for long-term troubleshooting.

In Elixir, the `IO.inspect/2` function is implemented using the `IO` module, which provides a variety of formatting options and allows for the printing of complex data structures.

## See Also:

- [Elixir IO module documentation](https://hexdocs.pm/elixir/IO.html)
- [Elixir Logger module documentation](https://hexdocs.pm/elixir/Logger.html)
- [Elixir debugging techniques](https://pragmaticstudio.com/tutorials/elixir-debugging-techniques)