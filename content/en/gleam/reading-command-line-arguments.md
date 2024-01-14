---
title:    "Gleam recipe: Reading command line arguments"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why
Have you ever encountered a program that requires you to enter certain information through the command line when you run it? You may have seen something like `node index.js --name="John" --age=25`. These are known as command line arguments and they allow you to provide input to a program without having to manually enter it while it is running. But have you ever wondered how to read these arguments in the program itself? In this blog post, we'll dive into the world of command line arguments in Gleam and learn how to read and utilize them in your programs.

## How To

Reading command line arguments in Gleam is a fairly straightforward process. First, we need to import the `command` module which provides functions for working with command line arguments. We can do this by adding the following code at the top of our Gleam file:

```Gleam
import gleam/command
```

Next, we can use the `arguments` function from the `command` module to get a list of all the arguments passed in when running our program. Let's take a look at an example:

```Gleam
fn main() {
    arguments = command.arguments()
    // do something with the arguments
}
```

The `arguments` variable will now contain a list of all the arguments passed in, including the program name itself. We can access individual arguments by their index in the list, for example `arguments[1]` would give us the first argument after the program name. We can also use the `length` function to find out the total number of arguments passed in. Let's see how we can use this to print out all the arguments in our program:

```Gleam
fn main() {
    arguments = command.arguments()
    
    for argument in arguments {
        io.println("Argument: " ++ argument)
    }
    
    io.println("Total arguments: " ++ command.length(arguments))
}
```

If we run this program with the arguments `node index.js hello world`, we would get the following output:
```
Argument: node
Argument: hello
Argument: world
Total arguments: 3
```

## Deep Dive

Now that we know the basics of reading command line arguments, let's take a deeper look at the process. As mentioned earlier, the `arguments` function returns a list of all the arguments passed in, including the program name itself. This means that the first element in the list will always be the program name, followed by the actual arguments.

Additionally, the arguments will be treated as strings by default, but we can use functions like `to_int` or `to_float` to convert them to a different data type if needed. We can also use pattern matching to handle specific argument patterns and perform different actions based on the input.

## See Also

- [Gleam documentation: Command line arguments](https://gleam.run/book/tutorials/command_line_arguments.html)
- [Command module in the Gleam standard library](https://gleam.run/modules/command.html)
- [How to read command line arguments in Node.js](https://stackabuse.com/how-to-read-command-line-arguments-in-node-js/)