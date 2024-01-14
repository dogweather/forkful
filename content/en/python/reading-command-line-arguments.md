---
title:    "Python recipe: Reading command line arguments"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why

In Python, command line arguments are an important concept to understand as they allow for more flexibility and customization in your code. By reading command line arguments, you can create programs that take in user input and perform specific actions based on that input. This can be useful for a variety of purposes, such as creating command line tools or building more interactive programs.

## How To
Reading command line arguments in Python is a relatively simple process. First, we need to import the `sys` module, which allows us to interact with the system and access command line arguments. Then, we can use the `sys.argv` attribute to access the arguments passed in when the program is run. Let's take a look at an example:

```Python
import sys

# Check if any arguments were passed in
if len(sys.argv) > 1:
    # Access the first argument (index 0 is the name of the file)
    argument = sys.argv[1]
    print("Your argument was:", argument)
else:
    print("No arguments passed in.")
```

Let's say we save this file as `arguments.py` and run it in the command line with `python arguments.py hello`. The output would be `Your argument was: hello`. However, if we run it without any arguments like `python arguments.py`, the output would be `No arguments passed in.`. As you can see, we were able to access the argument passed in and perform a specific action based on that input.

## Deep Dive

There are a few other things to note when it comes to reading command line arguments in Python. First, the `sys.argv` attribute returns a list, so you can access multiple arguments by indexing the list. Additionally, you can use the built-in `len()` function to determine the number of arguments passed in, as shown in the example above.

Another useful tool when it comes to command line arguments is the `argparse` module. This module allows for more advanced argument parsing, such as specifying argument types and adding optional arguments. It also provides built-in help and error messages for a more user-friendly experience. However, the `argparse` module is beyond the scope of this blog post, so be sure to check out the "See Also" section for more information.

## See Also
- [Python's `sys` module](https://docs.python.org/3/library/sys.html)
- [Python's `argparse` module](https://docs.python.org/3/library/argparse.html)