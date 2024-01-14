---
title:    "Gleam recipe: Reading command line arguments"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Why
Command line arguments are an essential part of any programming language, allowing users to provide input to a program directly from the terminal. In this blog post, we will explore how to read and handle these arguments in Gleam, a modern and functional programming language.

## How To
First, we need to define command line arguments in our `main` function. This is done by using the `argv` function from the `gleam/io` module. Here's an example of a simple `main` function with command line arguments:

```Gleam
import gleam/io

pub fn main(_args: List(String)) {
  let args = _args

  // rest of your program
}
```

Once we have defined our `args` variable, we can use it to access the command line arguments in our program. The `args` variable is a list of strings, with each element representing a command line argument passed by the user.

Let's look at an example of a program that reads in two command line arguments and prints them out:

```Gleam
import gleam/io

pub fn main(args: List(String)) {
  let first_arg = args[0]
  let second_arg = args[1]

  gleam/io.println(first_arg)
  gleam/io.println(second_arg)
}
```

If we run this program with `gleam run my_program.gleam first second` in the terminal, the output will be:

```
first
second
```

## Deep Dive
In addition to using the `argv` function, there are other functions in the `gleam/io` module that can be useful for handling command line arguments. For example:

- `length(args)` - to get the number of command line arguments passed
- `slice(start, end, args)` - to create a sublist of command line arguments
- `starts_with(prefix, args)` - to check if a command line argument starts with a given prefix

Additionally, Gleam also has the `gleam/env` module which can be useful for accessing environment variables, which can be used as command line arguments as well.

## See Also
- [Gleam docs on handling command line arguments](https://gleam.run/book/tutorials/commandline_arguments.html)
- [Learn Gleam](https://github.com/gleam-lang/learn-gleam)
- [Gleam Community Forum](https://community.gleam.run/)