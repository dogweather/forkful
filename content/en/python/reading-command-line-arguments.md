---
title:                "Python recipe: Reading command line arguments"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why

Learning how to read and utilize command line arguments is a valuable skill for any Python programmer. By understanding how to handle user input from the command line, you can create more dynamic and customizable programs. This can also make your code more user-friendly and easier to debug.

## How To

To read command line arguments in Python, you can use the `sys` module. Let's take a look at a simple example:

```
import sys

args = sys.argv
print("Number of arguments:", len(args))
print("Argument list:", args)
```

In this code, we import the `sys` module and use the `argv` attribute to access a list of all the arguments passed in when running the Python script. We can then use the `len()` function to determine the number of arguments and the `print()` function to display the argument list.

Let's say we save this code as `arguments.py` and run it in the command line with the arguments "hi" and "there" like this: `python arguments.py hi there`. The output would look like this:

```
Number of arguments: 3
Argument list: ['arguments.py', 'hi', 'there']
```

We can see that the first argument is always the name of the Python script itself, followed by any additional arguments passed in.

We can also access specific arguments by using their index in the list. For example, if we wanted to print out only the second argument ("hi") we could use `print(args[1])`.

## Deep Dive

The `sys.argv` list includes both the script name and any arguments, but sometimes we may only want to access the arguments. In this case, we can use the `argparse` module, which allows us to specify what arguments we are expecting and gives us more control over how they are handled.

Another useful feature is being able to pass in arguments with specific flags or options, like `-v` for verbose output. These are often used in large programs or scripts where there may be many different arguments to handle.

## See Also
- [Python Documentation on `sys` module](https://docs.python.org/3/library/sys.html)
- [Python Documentation on `argparse` module](https://docs.python.org/3/library/argparse.html)