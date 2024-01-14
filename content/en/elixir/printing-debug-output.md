---
title:    "Elixir recipe: Printing debug output"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

### Why

Debugging is an essential part of any programming language, and Elixir is no exception. In order to effectively debug our code, printing debug output can be an incredibly valuable tool. It allows us to see the values of variables at different points in our code and gives us a better understanding of how our program is functioning.

### How To

Printing debug output in Elixir is quite simple. We can use the `IO.inspect` function to print out the values of variables at any point in our code. Let's take a look at an example:

```Elixir
defmodule Math do
  def add(x, y) do
    IO.inspect x
    IO.inspect y
    x + y
  end
end

Math.add(5, 10)
```

In this example, we have a simple `add` function that takes in two parameters and adds them together. Before the addition, we use `IO.inspect` to print out the values of `x` and `y`. Running this code will produce the following output:

```Elixir
5
10
15
```

We can see that the values of `x` and `y` were printed out before being added together. This can be incredibly useful in identifying any unexpected values or errors in our code.

### Deep Dive

While `IO.inspect` is the most commonly used method for printing debug output, there are a few other options available in Elixir. These include `IO.puts` and `IO.warn` which can be useful in different scenarios. 

Additionally, Elixir provides us with the `Logger` module, which allows us to print out more detailed information about our code, such as the current process and line number. Using `Logger` can be especially helpful when working with concurrent processes in Elixir.

There are also various options for formatting the output, such as using `inspect` which displays the data in a more user-friendly format, or `IO.inspect` with the `pretty: true` option, which helps with readability of nested data structures.

### See Also

- [Elixir official documentation on debugging](https://elixir-lang.org/getting-started/debugging.html)
- [ElixirSchool article on IO module](https://elixirschool.com/en/lessons/basics/io/)
- [ElixirForum discussion on printing debug output](https://elixirforum.com/t/print-function-for-debugging/5487)