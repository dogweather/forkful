---
title:    "Python recipe: Writing to standard error"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Why

Writing to standard error, also known as stderr, is an important tool in Python programming. It allows developers to debug and troubleshoot their code by printing error messages to the terminal. This can help identify and fix issues in the code, making it an essential skill for any Python programmer.

## How To

Writing to standard error in Python is a simple process. All you need to do is import the `sys` library and use the `write()` function to print the error message. Here's an example:

```Python
import sys

sys.stderr.write("Oops, something went wrong!")
```

Running this code will display the error message "Oops, something went wrong!" in the terminal. It's important to note that these error messages do not stop the execution of the code, so you can continue to run and test your code.

Another useful feature of writing to stderr is that it allows you to customize your error messages. You can include information such as the line number or variable name where the error occurred, making it easier to locate and fix the issue. Here's an example of a customized error message:

```Python
import sys

x = 10
y = 0

try:
  result = x / y
except Exception as e:
  sys.stderr.write(f"Error: {type(e).__name__} - {e} on line 6")
```

The output of this code will be "Error: ZeroDivisionError - division by zero on line 6", providing valuable information about the error.

## Deep Dive

Writing to standard error uses the `stderr` file descriptor, which allows you to write to the terminal separately from the `stdout` file descriptor, which is used for regular output. This is why error messages can be displayed even if the `stdout` is redirected to a file.

It's important to note that the `stderr` stream is unbuffered, which means that error messages are printed immediately without waiting for other output to be displayed. This is useful when debugging because it ensures that the error message is shown as soon as it occurs.

## See Also

- [Python sys library documentation](https://docs.python.org/3/library/sys.html)
- [Writing to standard error in Python](https://cmdlinetips.com/2014/03/how-to-write-to-stderr/)
- [Understanding stderr, stdout, and stdin in Python](https://stackabuse.com/understanding-stderr-stdout-and-stdin-in-python/)