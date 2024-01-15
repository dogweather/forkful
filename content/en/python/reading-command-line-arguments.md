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

## Why

If you're new to programming, you may be wondering what command line arguments are and why they are important. Command line arguments allow you to pass information to your program before it runs, making it more dynamic and versatile.

## How To

To read command line arguments in Python, you can use the `sys` module. First, you need to import it into your script using the statement `import sys`. Then, you can access the arguments using `sys.argv`, which returns a list of strings.

Let's look at an example:

```Python
import sys
print("Total arguments:", len(sys.argv))

print("Argument List:", str(sys.argv))
```

Running the above code with the arguments `python example.py arg1 arg2` would produce the following output:

```
Total arguments: 3
Argument List: ['example.py', 'arg1', 'arg2']
```

You can also access individual arguments using their index in the list. For example, `sys.argv[1]` would return the first argument, which in this case is `'arg1'`.

## Deep Dive

In addition to basic usage, there are a few things to keep in mind when working with command line arguments in Python. Firstly, the script's name is included in the argument list, so `sys.argv[0]` will always return the script's name.

Additionally, arguments are always passed as strings, so if you need to use them as different data types, you will need to convert them using functions like `int()` or `float()`.

Lastly, it's important to handle errors when reading command line arguments, as unexpected inputs can cause your program to crash. Using `try` and `except` statements can help you handle these errors gracefully.

## See Also

For more information on command line arguments and the `sys` module, check out the following resources:

- [Official Python documentation for `sys` module](https://docs.python.org/3/library/sys.html)
- [Real Python's tutorial on command line arguments](https://realpython.com/python-command-line-arguments/)
- [Python Crash Course's chapter on command line arguments](https://ehmatthes.github.io/pcc_2e/regular_index/command_line_arguments.html)