---
title:    "Python recipe: Reading command line arguments"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Why

Command line arguments are an essential part of Python programming as they allow for user input and customization of code without the need for constant editing or re-running of the program. Learning how to read command line arguments can save time and effort in the long run.

## How To

Reading command line arguments in Python is a simple process. To start, we must import the "sys" library. This library allows for the use of the "argv" function, which holds the values of the arguments passed from the command line.

```
Python
import sys

# Storing the value of arguments in a variable
arguments = sys.argv

# Printing out the number of arguments passed
print("Number of arguments: ", len(arguments))

# Looping through the arguments and printing out their values
for i in range(len(arguments)):
  print("Argument", i, ":", arguments[i])
```

Sample output:
```
Number of arguments: 3
Argument 0 : filename.py
Argument 1: hello
Argument 2: world
```

In this example, we can see that the first argument (index 0) is the name of the Python file being run, followed by the two arguments "hello" and "world" that were passed from the command line.

## Deep Dive

Command line arguments are not limited to just words or strings. They can also be used to pass in numbers, lists, and even functions. To do so, the arguments must be converted from strings to their appropriate data types.

```
Python
import sys

# Getting a list of numbers from the command line
numbers = sys.argv[1:]
# Converting the strings to integers
numbers = [int(num) for num in numbers]

# Defining a simple function to add numbers
def add(x, y):
  return x + y

# Passing in the numbers from the command line as arguments to the function
sum = add(numbers[0], numbers[1])
print("The sum is:", sum)
```

Sample output:
```
Number of arguments: 3
Argument 0 : filename.py
Argument 1: 5
Argument 2: 10
The sum is: 15
```

In this example, we are passing two numbers (5 and 10) as command line arguments and using them to perform addition in our function. This allows for more flexibility and customization in our code.

## See Also

- [Python Documentation on sys library](https://docs.python.org/3/library/sys.html)
- [RealPython guide on command line arguments in Python](https://realpython.com/python-command-line-arguments/)
- [Python argparse module for more advanced command line argument handling](https://docs.python.org/3/library/argparse.html)