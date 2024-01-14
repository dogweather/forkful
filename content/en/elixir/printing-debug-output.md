---
title:                "Elixir recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why 

Debugging is an essential part of the development process in any programming language. It allows developers to identify and fix issues in their code. However, sometimes debugging can be a challenging and time-consuming task. This is where printing debug output comes in handy. By printing out specific values or messages during the execution of our code, we can better understand its flow and identify any potential bugs or errors.

## How To 

To print debug output in Elixir, we can use the `IO.inspect/2` function. This function takes in two arguments - the value we want to print and an optional keyword list for formatting options. Let's see an example:

```Elixir
my_list = [1, 2, 3]
IO.inspect(my_list)
```

The output of this code would be:

```
[1, 2, 3]
```

We can also use string interpolation in the `IO.inspect/2` function to print out a message along with the value. Here's an example:

```Elixir
name = "John"
IO.inspect("Hello, my name is #{name}")
```

The output of this code would be:

```
"Hello, my name is John"
```

## Deep Dive 

There are a few important things to keep in mind when printing debug output in Elixir. First, it's essential to use the `IO.inspect/2` function only during development. It should not be left in the code in production as it can cause performance issues. 

Secondly, we can also use the `IO.inspect/2` function to print out the values of variables in a specific function. This can be helpful in cases where we want to monitor the state of a variable throughout the execution of a function.

Lastly, there are additional options available for formatting the output of the `IO.inspect/2` function, such as specifying the color or depth of the output.

## See Also 

- [Elixir official documentation on IO.inspect/2](https://hexdocs.pm/elixir/IO.html#inspect/2)
- [Official Elixir debugging guide](https://elixir-lang.org/getting-started/debugging.html)
- [Code School's guide to debugging in Elixir](https://www.codeschool.com/articles/getting-started-with-debugging-in-elixir)

By using the `IO.inspect/2` function, printing debug output in Elixir becomes a straightforward and efficient method for debugging your code. So next time you're stuck on a bug, try adding some debug output and see how it helps in identifying the issue. Happy coding!