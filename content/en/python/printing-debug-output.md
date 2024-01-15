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

## Why

Debugging is an essential part of the programming process, especially when dealing with complex code. Printing debug output is a simple yet effective way to gain insight into what is happening in your code and identify any errors or unexpected behavior.

## How To

To print debug output in Python, we can use the built-in `print()` function. This function allows us to display messages on the screen as our code is running, giving us a closer look at the values of variables and the flow of our program.

```Python
num1 = 10
num2 = 5

print("The value of num1 is:", num1)
print("The value of num2 is:", num2)
```

This code will output the following:

```
The value of num1 is: 10
The value of num2 is: 5
```

We can also use the format string feature to display more complex debug messages.

```Python
name = "John"
age = 25
height = 6.3

print(f"Hello, my name is {name}. I am {age} years old and {height} feet tall.")
```

This will output:

```
Hello, my name is John. I am 25 years old and 6.3 feet tall.
```

Debug output can also be used to check if a specific condition is being met within our code.

```Python
num = 12
if num > 10:
  print("The number is greater than 10.")
else:
  print("The number is less than 10.")
```

The output of this code will be:

```
The number is greater than 10.
```

## Deep Dive

Printing debug output not only helps us identify issues in our code but also provides us with valuable information about its execution. By strategically placing print statements throughout our code, we can better understand the flow and logic behind it.

One tip for effective debugging is to use descriptive messages in our print statements. This makes it easier to track which messages are coming from which parts of our code and can save time in the debugging process.

Another useful technique is to use the `logger` module in Python, which provides more advanced options for logging and debugging. It allows us to specify different levels of log messages and also offers features like timestamps and file output.

## See Also

For more information on debugging in Python, here are some useful resources:

- [Python For Beginners: Debugging Your Code](https://www.pythonforbeginners.com/debugging/debugging-your-code)
- [Debugging in Python: A Pragmatic Approach](https://realpython.com/python-debugging-pdb/)
- [Logging in Python](https://realpython.com/python-logging/)
- [Debugging Tips and Tricks in Python](https://hackernoon.com/debugging-tips-and-tricks-in-python-lx29rxiz)