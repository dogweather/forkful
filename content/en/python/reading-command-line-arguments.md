---
title:                "Python recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why

When writing programs in Python, it's common to use command line arguments as inputs. These arguments allow users to pass in specific information when running the program, making it more versatile and customizable. Understanding how to read command line arguments is an important skill for any Python programmer.

## How To

To read command line arguments in Python, we can use the `sys` library, which provides access to system-specific parameters and functions. First, we import the library:

```Python
import sys
```

Next, we can access the command line arguments using the `argv` variable from the `sys` library:

```Python
args = sys.argv
```

The `args` variable now stores a list of all the arguments passed in when running the program. We can access specific arguments by their index in the list, starting at 0. For example, if our program was called with the arguments `python program.py argument1 argument2`, we can access `argument1` by using `args[1]` and `argument2` by using `args[2]`.

To see this in action, let's create a simple program that prints out the arguments passed in:

```Python
import sys

args = sys.argv

print("Arguments passed in: ", args)
print("First argument: ", args[1])
print("Second argument: ", args[2])
```

When we run this program with `python program.py hello world`, we get the following output:

```
Arguments passed in:  ['program.py', 'hello', 'world']
First argument:  hello
Second argument:  world
```

We can also manipulate the arguments in our program, such as converting them to integers or performing calculations with them.

## Deep Dive

While the `sys` library is a commonly used method for reading command line arguments, there are other libraries and methods available such as `argparse` and `getopt`. These provide more advanced functionalities and options for parsing and handling command line arguments.

It's important to keep in mind that command line arguments can be sensitive to special characters or spaces, so proper handling and validation is necessary to avoid any unexpected errors.

Additionally, command line arguments can also be used in conjunction with built-in libraries like `os` and `subprocess` for performing system-level tasks and executing external commands.

## See Also

- [Python sys library documentation](https://docs.python.org/3/library/sys.html)
- [Argparse tutorial](https://realpython.com/command-line-interfaces-python-argparse/)
- [Command line arguments with getopt](https://www.geeksforgeeks.org/command-line-arguments-in-python/)
- [Using command line arguments with os and subprocess](https://stackabuse.com/executing-shell-commands-with-python/)