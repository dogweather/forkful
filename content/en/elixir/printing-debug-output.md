---
title:                "Elixir recipe: Printing debug output"
programming_language: "Elixir"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why

Have you ever encountered a bug in your Elixir code and spent hours trying to figure out where the issue lies? Printing debug output is a useful technique that can help you identify and fix bugs more efficiently. In this blog post, we will explore the importance of printing debug output and how to effectively use it in your Elixir code.

## How To

To print debug output in Elixir, we can use the `IO.inspect/2` function. This function takes two arguments: the value we want to inspect and an optional message to display alongside the output.

```Elixir
defmodule MyModule do
  def add(a, b) do
    result = a + b
    IO.inspect(result, label: "Result is")
    result
  end
end

MyModule.add(2, 3)

```

The above code will output the following:

```
Result is: 5
```

We can also use interpolation in our message to display the values of variables at a specific point in our code.

```Elixir
defmodule MyModule do
  def say_hello(name) do
    message = "Hello #{name}!"
    IO.inspect(message, label: "Message:")
  end
end

MyModule.say_hello("John")

```

The output will be:

```
Message: Hello John!
```

This can be especially helpful in larger codebases where it may be difficult to keep track of variable values.

## Deep Dive

One thing to keep in mind when printing debug output is that it can slow down your code. This is because the function needs to convert the value to a string before displaying it. To avoid this performance impact, we can use `IO.inspect/2` with the `:label` option set to `nil`.

```Elixir
defmodule MyModule do
  def factorial(n) do
    if n == 1 do
      result = 1
    else
      result = n * factorial(n-1)
    end
    IO.inspect(nil, label: "Current value:")
    result
  end
end

MyModule.factorial(5)

```

In this example, we are only printing the label without the actual value, thus avoiding the performance impact.

Another useful function for printing debug output is `IO.puts/2`, which simply outputs the given value as a string without any additional formatting. This can be helpful when debugging complex data structures, as it will print the entire structure on a single line.

## See Also

- [Elixir School - Debugging](https://elixirschool.com/en/lessons/basics/debugging/)
- [Elixir Documentation - IO module](https://hexdocs.pm/elixir/IO.html)
- [Debugging with `IO.inspect` in Elixir](https://dev.to/afreeorange/debugging-with-io-inspect-in-elixir-4ck0)

Happy debugging!