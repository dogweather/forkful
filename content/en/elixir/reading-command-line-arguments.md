---
title:                "Elixir recipe: Reading command line arguments"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why 

Have you ever run a program from the command line and wondered how it was able to read the arguments you passed in? Or perhaps you're working on a project that requires you to handle command line arguments and you're not sure where to start. Either way, understanding how to read command line arguments in Elixir can be a useful skill for any developer. So let's dive in and learn how!

## How To

First, let's start with a basic example. Let's say we have a file called `greet.ex` and we want to pass in a name as an argument and have our program greet that person. Here's how we can do that:

```Elixir
# greet.ex
name = System.argv[1]
IO.puts "Hello #{name}!"
```

Now when we run our program from the command line, for example `elixir greet.ex John`, we'll see the output `Hello John!`.

But what if we want to pass in multiple arguments? Not a problem! We can use the `System.argv` function to read in arguments as a list. Here's an example:

```Elixir
# multiply.ex
arguments = System.argv

first_number = String.to_integer(arguments[1])
second_number = String.to_integer(arguments[2])

product = first_number * second_number
IO.puts "The product of #{first_number} and #{second_number} is #{product}."
```

Now when we run our program with the command `elixir multiply.ex 4 5`, we'll see the output `The product of 4 and 5 is 20`. Pretty cool, right?

## Deep Dive

So let's take a deeper look at how `System.argv` works. This function returns a list of all the arguments passed in, including the program name itself. The first element in the list will always be the name of the program, so if we want to access just the arguments, we need to start at the second element, which is at index 1. We can also use functions like `String.to_integer` to convert our arguments to different data types, as we did in our multiplication example.

Another thing to keep in mind is that command line arguments are always passed in as strings. So if you want to perform any type of mathematical operation on the arguments, you'll need to convert them to numbers first.

## See Also

For more information on working with command line arguments in Elixir, check out these resources:

- Elixir's [System](https://hexdocs.pm/elixir/System.html) module documentation
- Elixir School's [Lesson on Command Line Applications](https://elixirschool.com/en/lessons/advanced/command-line-args/)
- Elixir Forum's [Thread on Reading Command Line Arguments](https://elixirforum.com/t/how-to-read-arguments-from-the-command-line/10961)

So there you have it, a quick and easy guide to reading command line arguments in Elixir. Happy coding!