---
title:                "Reading command line arguments"
html_title:           "C++ recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?

Reading command line arguments in Python means getting the input from the user directly from the command line when running the program. It's a must-have for command-line utilities and scripts, to allow for customization on each run without changing the program's code.

## How to:
Python's build-in library "sys" provides access to command-line arguments with its `argv` attribute. The first element, `argv[0]`, is always the script name. Succeeding elements are arguments passed. Here's simple code to print all arguments:

```Python
import sys

print(sys.argv)
```
Running `python hello.py arg1 arg2` gives you `['hello.py', 'arg1', 'arg2']`.

For detailed parsing, use the `argparse` module:

```Python
import argparse

parser = argparse.ArgumentParser(description="A simple argument parser")
parser.add_argument('-n','--name', help='Your Name', required=True)

args = vars(parser.parse_args())
print('Hello ' + args['name'])
```
Running `python hello.py --name John` prints `Hello John`.

## Deep Dive
Python was born Unix-based, where command-line arguments were important, hence `sys.argv` was included from the start. Later, the more advanced `argparse`  was added in Python 2.7.

Alternatives to reading command line arguments are environment variables, file inputs, or hardcoded values. These methods, however, lack the convenience, flexibility, and ease-of-use that command line arguments present.

`sys.argv` is a list of strings, `argparse` parses these strings into more user-friendly types (integers, booleans, etc.) and provides useful feedback and the automatic `--help` option.

## See Also
- Python's [argparse](https://docs.python.org/3/library/argparse.html) tutorial.
- [sys module documentation](https://docs.python.org/3/library/sys.html).
- A handy guide on command line arguments at [Real Python](https://realpython.com/python-command-line-arguments/).