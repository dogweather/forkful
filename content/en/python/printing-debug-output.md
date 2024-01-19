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

Printing debug output is a method where a programmer outputs the status of a program to console or file. It's an invaluable tool for diagnicating issues, understanding program flow, and unearthing potentially hidden bugs.

## How to:

Printing debug output in Python is as simple as using the built-in `print()` method. But `logging` module allows more flexibility and control.

```Python
# Basic print statement
x = 10
print("Value of x is:", x)

# Debugging using logging
import logging
logging.basicConfig(level=logging.DEBUG)
logging.debug("Value of x is: %s", x)
```

Output of both commands will be `Value of x is: 10`.

## Deep Dive:

Printing debug output is an age-old technique of debugging. It can date back to the dawn of programming itself. In Python, while `print` is straightforward, `logging` allows output to various targets, customizable format, and filtering logs by severity.

Alternatives of printing debug output are debuggers like pdb or IDE integrated debuggers. They offer more 'real-time' insight into the program.

Implementation-wise, `print` simply outputs to sys.stdout while `logging` consists of several classes like Logger, Handler, Filter, and Formatter, forming a more complex system that can handle different scenarios.

## See Also:

- Official Python Documentation on Logging: https://docs.python.org/3/library/logging.html
- Python tips on effective logging: https://realpython.com/python-logging/
- Brief history of debugging: https://www.toptal.com/developers/blog/the-evolution-of-debugging
- Using pdb in Python: https://docs.python.org/3/library/pdb.html