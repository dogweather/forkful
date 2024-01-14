---
title:    "Elixir recipe: Printing debug output"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Why
Debugging is an essential part of programming, allowing developers to identify and fix issues in their code. One useful tool in the debugging process is printing debug output. By printing specific values or messages during runtime, developers can gain insight into the behavior of their code and pinpoint any errors or unexpected results.

## How To
Printing debug output in Elixir is a straightforward process. In this section, we will go through some coding examples to demonstrate how to print debug output in different scenarios.

```Elixir
# Printing a single value
IO.inspect("Hello World")
# Output: "Hello World"

# Printing multiple values
IO.inspect({2 + 3, "Elixir"})
# Output: {5, "Elixir"}

# Printing inside a function
def add(x, y) do
  result = x + y
  IO.inspect("The result is: #{result}")
  result
end
add(5, 2)
# Output: "The result is: 7"
```

As you can see, the `IO.inspect` function allows us to print any value or expression to the terminal. It also returns the value, making it useful when used inside other functions.

## Deep Dive
While `IO.inspect` is a handy tool, there are a few things to keep in mind when using it for debugging. First, it is essential to note that printing debug output can affect the performance of your code. If you have a large amount of output, it can slow down your application.

Another thing to consider is the use of `IO.puts` vs. `IO.inspect`. While both functions print to the terminal, `IO.puts` only accepts strings and will not return the value. On the other hand, `IO.inspect` can print any value or expression and return the value.

Finally, you can also pass a second argument to `IO.inspect` to specify the level of depth to print for nested data structures. By default, it has a depth of 5, but you can increase or decrease this according to your needs.

## See Also
- [Elixir Debugging Techniques](https://elixir-lang.org/getting-started/debugging.html)
- [Inspect module documentation](https://hexdocs.pm/elixir/Inspect.html)
- [Debugging in Elixir with Pry](https://medium.com/@mobileoverlord/debugging-your-elixir-code-with-pry-a1ba13ff59e5)