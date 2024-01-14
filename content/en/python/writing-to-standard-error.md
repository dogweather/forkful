---
title:    "Python recipe: Writing to standard error"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why

In Python programming, there are many different ways to print information for debugging and troubleshooting purposes. One common method is printing to standard error, which can provide more detailed and specific information than just using the standard print function. In this blog post, we will explore why and how to use standard error in your Python code.

## How To

To write to standard error in Python, you can use the `sys` module which contains the `stderr` object. Here's an example:

```Python
import sys

sys.stderr.write("There was an error!")
```

This will print the message "There was an error!" to the standard error output. You can also use the `>>` operator to redirect the output to standard error, like this:

```Python
print("This will print to standard output")
print("This will also print to standard output")

print("This will print to standard error", file=sys.stderr)
```

The above code will produce the following output:

```
This will print to standard output
This will also print to standard output
This will print to standard error
```

You can see that the message "This will print to standard error" was printed on a separate line and in a different color, indicating that it was written to standard error.

## Deep Dive

When you use the `print` function in Python, by default it will write to standard output (also referred to as `sys.stdout`). This is useful for displaying the results of your code to the user, but it may not always be the best option for debugging. That's where using standard error comes in handy. Writing to standard error ensures that your debugging messages are not mixed in with the output of the program, making it easier to identify and track down errors.

You can also use standard error in conjunction with the `logging` module in Python. This can give you even more control over the format and level of your debugging messages. By using the `logging` module, you can also write to log files instead of just printing to the console.

## See Also

Here are some additional resources for learning more about writing to standard error in Python:

- [Official Python Documentation on sys.stderr](https://docs.python.org/3/library/sys.html#sys.stderr)
- [Python Logging Tutorial](https://realpython.com/python-logging/)
- [Understanding Standard Input, Output, and Error Streams](https://www.linuxjournal.com/content/understanding-streams-linux)