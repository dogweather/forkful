---
title:                "Printing debug output"
html_title:           "Python recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?

Printing debug output is a way for programmers to track and understand the flow of their code while it is running. It allows them to see the values of variables and the execution of functions throughout the program.

Programmers use printing debug output to catch errors and troubleshoot issues in their code. By logging important information, they can identify where a problem may be occurring and make necessary changes to fix it.

## How to:

```Python
# To print a variable's value:
x = 3
print(x) # Output: 3

# To log a message:
print("Hello World!") # Output: Hello World!

# To print multiple values:
a = 2
b = 5
print("a =", a, "and b =", b) # Output: a = 2 and b = 5

# To format output:
name = "John"
age = 26
print("My name is {} and I am {} years old.".format(name, age)) # Output: My name is John and I am 26 years old.
```

## Deep Dive:

When programming was first introduced, debugging was done by physically checking and removing errors in the hardware. However, with the advent of high-level languages, printing debug output has become a crucial tool for all programmers.

An alternative to printing debug output is using a debugger, which is a software tool that allows developers to step through their code line by line to understand its execution. Another alternative is using logging libraries, which offer more extensive features for tracking and managing log messages.

In Python, there are two main methods for printing debug output - `print()` and `logging`. The `print()` function is built-in and allows for easy and quick debugging. The `logging` library offers more advanced features such as custom logging levels and formatting.

## See Also:

- [Python Official Documentation on Debugging](https://docs.python.org/3/debugging.html)
- [Real Python's Guide to Debugging Python Programs](https://realpython.com/python-debugging-pdb/)
- [How Debugging is Used in the Software Development Process](https://www.algoworks.com/blog/debugging-in-software-development-process/)