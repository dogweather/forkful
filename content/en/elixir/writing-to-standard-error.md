---
title:                "Elixir recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why

Have you ever encountered an error while writing code in Elixir and wondered how to properly handle it? Writing to standard error is a helpful way to capture and display these errors, making it easier to debug and find solutions for. In this blog post, we will explore the basics of writing to standard error in Elixir and how it can improve your coding experience.

## How To

To write to standard error in Elixir, we can use the `IO.write/2` function and specify `:stderr` as the second argument. Let's take a look at an example:

```Elixir
IO.write(:stderr, "This is an error message.")
```
Running this code in the Elixir terminal will output `This is an error message.` in red text, indicating that it was written to standard error. 

In the code block above, we use the `IO.write/2` function to write the message to the specified output, in this case `:stderr`. You can also use the `IO.puts/2` function in the same way, which adds a new line after the message. 

Let's try another example to see how we can handle errors using the `IO.write/2` function:

```Elixir
try do
  file = File.open("nonexistentfile.txt")
  IO.write(:stderr, "File opened successfully.")
catch
  error -> IO.write(:stderr, "Error: #{inspect error}")
end
```

In this example, we open a file that does not exist, triggering an `error` in the `catch` block. Using `IO.write/1`, we can output a custom error message along with the specific `error` that occurred.

## Deep Dive

Behind the scenes, Elixir uses the `:stdio` module to handle standard input, output, and error. When we use `:stderr` as the second argument in the `IO.write/2` function, we are essentially telling Elixir to write the message to the error output handled by the `:stdio` module.

It is important to note that writing to standard error will not interrupt the execution of the program, but will simply display the error message in the terminal. This allows us to continue with our program and handle errors in a more organized and visible way.

## See Also

Below are some additional resources for learning more about writing to standard error in Elixir:

- [Elixir Documentation on IO.write/2 function](https://hexdocs.pm/elixir/IO.html#write/2)
- [Elixir Documentation on standard I/O](https://elixir-lang.org/getting-started/io-and-the-file-system.html#standard-io)
- [Elixir School](https://elixirschool.com/lessons/basics/io/) - a comprehensive learning resource for Elixir, including a lesson on I/O.

Now that you have a better understanding of writing to standard error in Elixir, give it a try in your own coding projects and see how it can improve your debugging process. Happy coding!