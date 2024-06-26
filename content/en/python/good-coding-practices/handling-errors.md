---
date: 2024-01-21 21:19:35.648555-07:00
description: 'How to: Sample output when entering an invalid number for the first
  block.'
lastmod: '2024-04-05T21:53:35.397976-06:00'
model: gpt-4-1106-preview
summary: Sample output when entering an invalid number for the first block.
title: Handling errors
weight: 16
---

## How to:
``` Python
# Basic try-except block
try:
    # risky code
    number = int(input("Enter a number: "))
except ValueError:
    # handle error
    print("That's not a number!")

# Specifying multiple exceptions
try:
    # code that might raise different exceptions
    result = 10 / int(input("Enter a divisor: "))
except ZeroDivisionError:
    print("Oops! Can't divide by zero.")
except ValueError:
    print("I need a number, buddy.")

# Using else and finally
try:
    number = int(input("Enter a number for squaring: "))
except ValueError:
    print("I said a number!")
else:
    # no errors occurred
    print("Your number squared is:", number**2)
finally:
    # always executes
    print("Thanks for trying this out!")
```

Sample output when entering an invalid number for the first block:
```
Enter a number: hello
That's not a number!
```

## Deep Dive
Since the dawn of programming, error handling has been crucial. Early approaches were rudimentary, like checking conditions before every risky operation. Python’s `try-except` syntax came from a heritage of exception handling in older languages like C++ and Java, simplifying the process.

When you `try` a block of code, Python watches for any exceptions. If an error pops up, the `except` block catches it. You can get specific about the exceptions you catch or catch ‘em all with a bare `except`. However, specifics first is the better approach – it’s precise, not a catch-all net.

`else` and `finally` are extras in this concept. The `else` block runs if the try block is error-free. `finally` is the reliable buddy that runs no matter what – think cleanup operations.

Alternatives? There sure are. Some languages use return codes instead of exceptions. You might also encounter `with` statements for handling resources or `assertions` that check conditions while developing. But when we talk about solid error-handling strategies, the try-catch model stands out for its readability and structure.

## See Also
Here are some good additional resources for diving even deeper:

- Python's official documentation on errors and exceptions: [Python Docs – Errors and Exceptions](https://docs.python.org/3/tutorial/errors.html)
- Real Python’s guide on the subject: [Real Python - The try/except/else/finally block](https://realpython.com/python-exceptions/)
- A thoughtful discussion on error handling best practices: [Stack Overflow – How do I properly ignore exceptions?](https://stackoverflow.com/questions/4990718/about-catching-any-exception)
