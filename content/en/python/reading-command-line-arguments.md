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
Reading command-line arguments in Python allows you to pass values or flags to your script during its execution. It provides dynamic user input, making your scripts more flexible and interactive.

## How to:
Python's built-in module `sys` is your go-to toolkit for command line arguments. It has a list `argv` that holds the command line arguments.

Here is a basic example to illustrate usage:

```Python
import sys

def main():
    print(f'Name of script: {sys.argv[0]}')
    for arg in sys.argv[1:]:
        print(f'Argument: {arg}')

if __name__ == '__main__':
    main()
```

Now, run this program in your terminal with some command line arguments:

```sh
$ python3 myscript.py arg1 arg2
```

The output will be:

```
Name of script: myscript.py
Argument: arg1
Argument: arg2
```

`sys.argv[0]` is the name of the script itself, and `sys.argv[1:]` contains the rest of the arguments.

## Deep Dive
Historically, command line arguments in Unix-like systems have been the main way programmers supply parameters to their scripts and programs. Python maintains this traditional versatility.

An alternative approach is using libraries like `argparse` or `getopt`. They provide more sophisticated parsing capabilities and automatic help messages, but with an increase in complexity.

Fun fact about implementation: `sys.argv`, under the hood, is just a list. Python's interpreter grabs those arguments when your script is initiated, and stores them in the `argv` list.

## See Also
More info about modules mentioned:
- sys: [python docs - sys](https://docs.python.org/3/library/sys.html)
- argparse: [python docs - argparse](https://docs.python.org/3/library/argparse.html)
- getopt: [python docs - getopt](https://docs.python.org/3/library/getopt.html)