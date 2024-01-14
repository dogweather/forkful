---
title:                "Python recipe: Printing debug output"
programming_language: "Python"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why

Debugging is an essential part of programming. It allows us to identify and fix errors in our code, leading to a more efficient and bug-free program. One of the most helpful tools in debugging is printing debug output. This feature allows us to see the values of variables and the flow of our code, providing valuable insights into how our program is running.

## How To

To print debug output in Python, we can use the `print()` function. We can pass in variables as arguments to print their values, or we can also print out helpful messages to track the flow of our code.

```Python
# Printing variable values
x = 10
print("The value of x is:", x)

# Tracking code flow
print("Starting program")
print("Initializing variables...")
a = 5
b = 7
print("Calculating result...")
result = a + b
print("The result is:", result)
print("Ending program")
```

The output of this code would be:
```
The value of x is: 10
Starting program
Initializing variables...
Calculating result...
The result is: 12
Ending program
```

We can also format our debug output to make it more readable and organized. The `format()` method allows us to insert values into a string, making it easier to understand the output.

```Python
# Formatting debug output
name = "John"
age = 25
job = "developer"
print("User name: {}, Age: {}, Job: {}".format(name, age, job))
```

The output of this code would be:
```
User name: John, Age: 25, Job: developer
```

## Deep Dive

There are a few things to keep in mind when printing out debug output. First, we should be careful not to clutter our code with too many print statements. This can make it harder to read and debug in the long run. It's best to strategically place print statements in areas where we believe there may be errors.

Another useful tip is to use the `__name__` variable to print out the name of the module or script we are currently running. This can be helpful in identifying which part of our code is being executed.

Finally, we can also use the `logging` module to print out debug output. This offers more functionality and control over our output, such as setting different levels of information to be printed.

## See Also

- [Python Debugging Techniques](https://realpython.com/python-debugging/)
- [The print() function in Python](https://www.geeksforgeeks.org/print-function-in-python/)
- [Python Logging Tutorial](https://www.pythonforbeginners.com/logging/python-logging-tutorial)