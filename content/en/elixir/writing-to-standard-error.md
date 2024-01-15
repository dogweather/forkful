---
title:                "Writing to standard error"
html_title:           "Elixir recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why

Writing to standard error may not be the most exciting topic, but it is a crucial aspect of programming in Elixir. As developers, we often rely on standard error to understand and debug issues in our code. By learning how to write to standard error, we can gain a better understanding of our programs and improve our overall development process.

## How To

The process of writing to standard error in Elixir is simple and straightforward. All we need to do is use the `IO.puts/2` function with a second argument of `:stderr` to specify that we want to write to standard error instead of standard output. Let's take a look at an example:

```Elixir
IO.puts("Hello World", :stderr)
```

When we run this code, we will see the message "Hello World" printed to our terminal as an error. This may not seem useful on its own, but we can leverage this functionality to provide more informative error messages in our programs.

For example, if we encounter an error in our program, we can use `IO.puts/2` to write a specific error message to standard error. This will help us pinpoint the issue and understand what went wrong in our code.

```Elixir
if input == "invalid" do
  IO.puts("Invalid input! Please try again.", :stderr)
end
```

## Deep Dive

In Elixir, standard error is represented by the `:stderr` atom. This atom is used to identify where error messages should be written to in the underlying operating system.

We can also use `IO.inspect/2` to write data to standard error for debugging purposes. This function prints the given data to standard error, which can help us understand and troubleshoot our code.

```Elixir
list = [1, 2, 3]
IO.inspect(list, label: "My List:", out: :stderr)
```

We can also use the `Kernel.raise/1` function to raise an error and write a custom message to standard error at the same time. This is useful when we want to explicitly raise an error in our code and provide more information about the issue.

```Elixir
defmodule Math do
  @doc "Returns the result of dividing `a` by `b`."
  def divide(a, b) do
    if b == 0 do
      IO.puts("Cannot divide by 0!", :stderr)
      Kernel.raise("Division by zero error!")
    end
    a / b
  end
end
```

## See Also

- [Elixir Documentation on IO](https://hexdocs.pm/elixir/IO.html)
- [Elixir Documentation on Kernel](https://hexdocs.pm/elixir/Kernel.html)
- [Article on Building CLI Tools in Elixir](https://bryanconner.com/building-cli-tools-in-elixir/)