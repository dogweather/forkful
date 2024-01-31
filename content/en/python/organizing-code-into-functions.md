---
title:                "Organizing code into functions"
date:                  2024-01-25T02:59:34.177778-07:00
model:                 gpt-4-1106-preview
simple_title:         "Organizing code into functions"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## What & Why?
Organizing code into functions is about breaking down your code into reusable chunks with specific purposes. We do it to make code cleaner, easier to read, debug, and update. 

## How to:
Let's say you're writing a script to calculate the square and cube of a number. Without functions, it's a mess of repetition:

```Python
num = 4
square = num * num
cube = num * num * num
print(f"Square: {square}, Cube: {cube}")

num = 5
square = num * num
cube = num * num * num
print(f"Square: {square}, Cube: {cube}")
```
Output:
```
Square: 16, Cube: 64
Square: 25, Cube: 125
```

With functions, it's neater:

```Python
def square(n):
    return n * n

def cube(n):
    return n ** 3

num = 4
print(f"Square: {square(num)}, Cube: {cube(num)}")

num = 5
print(f"Square: {square(num)}, Cube: {cube(num)}")
```
Output:
```
Square: 16, Cube: 64
Square: 25, Cube: 125
```

## Deep Dive
Back in the day, when programs were simple, you could get away with just writing a list of instructions. But as the software got more complex, developers realized they were rewriting the same code over and over. Hello, functions—reusable blocks of code that perform a single action.

Alternatives to functions include classes (bundling functions with data they operate on) and inline code (intelligence right where you need it, but risky for complex tasks). Implementation-wise, the trick is not just to create functions but to make them do one thing well—think single responsibility principle. Functions should also ideally be stateless, meaning no surprises with data coming in or going out.

## See Also
- The official Python tutorials on functions: https://docs.python.org/3/tutorial/controlflow.html#defining-functions
- 'Clean Code' by Robert C. Martin, for principles on how to write clean functions.
- 'Refactoring: Improving the Design of Existing Code' by Martin Fowler, which includes examples of organizing code.
