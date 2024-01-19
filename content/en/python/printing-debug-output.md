---
title:                "Printing debug output"
html_title:           "Arduino recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?

Printing debug output essentially means seeing intermediary results of your code. Why do that? It's your lifeline for diagnosing bugs, scrutinizing code behavior, and making sense of the black box that is your program.

## How to:

Outfitting debug messages in Python is as simple as using the `print()` function. You tell Python what to print within parentheses.

For instance, if you want to see the iteration over a list:

```Python
fruits = ["apple", "banana", "cherry"]
for x in fruits:
  print(x)
```

It'll gift you with:

```Python
apple
banana
cherry
```

If you're debugging a function, use `print()` to show variables’ state:

```Python
def add(x, y):
  print("x is", x)
  print("y is", y)
  return x + y

result = add(15, 27)
```

The output is:

```Python
x is 15
y is 27
```

Maintain those peepers peeled for these lines during your debug session. 

## Deep Dive

"Debugging" is not new - it dates back to the 1940s but the principles remain (i.e., finding and squashing bugs). Printing debug output remains a popular method even as more advanced tools (like debuggers & profilers) are hitting the shelves.

As an alternative to the primitive `print()` function, Python has a sophisticated logging module. It offers granulated control over what gets printed, like setting levels of severity and routing messages to different outputs.

```Python
import logging

logging.info('This is an info message')
logging.debug('This is a debug message')
```
The catch? By default, only messages with severity `warning` or above are displayed.

Want another alternative? Check the pdb module. The Python Debugger lets you interactively poke around while your program runs.

Considerations on implementation details vary between methods. For instance, `print()` puts output into `stdout`, not `stderr `- an important distinction when redirecting output.

## See Also

Highly recommend the Python docs for more extensive information about [logging](https://docs.python.org/3/library/logging.html) and [pdb debugger](https://docs.python.org/3/library/pdb.html). Keep exploring and Happy Debugging!