---
title:                "Reading command line arguments"
html_title:           "Python recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?

In simple terms, reading command line arguments means accepting user input from the command line when running a Python program, instead of hard-coding values directly into the code. Programmers do this to make their program more flexible and adaptable, allowing users to provide different input values without having to change the code every time.

## How to:

To read command line arguments in Python, we use the `sys` module and its `argv` variable. First, we import the module using `import sys`. Then, we can access the argument values using `sys.argv`, which returns a list of strings. The first element of the list (index 0) will always be the name of the Python file being executed. The subsequent elements are the values entered by the user.

```Python
import sys

# Sample code to read two command line arguments and print them out
# python sample.py argument1 argument2

print("Argument 1:", sys.argv[1])  # Output: Argument 1: argument1
print("Argument 2:", sys.argv[2])  # Output: Argument 2: argument2
```

## Deep Dive:

Reading command line arguments has been a common practice in programming since the early days of computers when programs were executed through a command line interface. It allows for a more interactive experience and also makes programs more customizable for different users.

An alternative to using the `sys` module is to use the `argparse` module, which provides more robust functionalities for parsing and validating command line arguments. It also makes the code more readable and organized, with the ability to add descriptions and help texts for each argument.

The implementation of `sys.argv` works by taking the values entered by the user and storing them in a list of strings. This list is then accessible to the program to use as needed. The values can also be converted to different data types using type casting.

## See Also:

- Official Python documentation on command line arguments: https://docs.python.org/3/library/sys.html#sys.argv
- More information on the `argparse` module: https://docs.python.org/3/library/argparse.html
- A detailed tutorial on using command line arguments in Python: https://realpython.com/command-line-interfaces-python-argparse/