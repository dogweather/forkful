---
title:                "Refactoring"
aliases:
- /en/python/refactoring/
date:                  2024-01-25T02:12:22.396984-07:00
model:                 gpt-4-1106-preview
simple_title:         "Refactoring"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/refactoring.md"
---

{{< edit_this_page >}}

## What & Why?
Refactoring is the process of restructuring existing computer code—changing the factoring—without changing its external behavior. Programmers do it to clean up code, improve readability, and make it easier to maintain and extend, all without adding new features.

## How to:
Suppose you've got a chunk of code that calculates and prints the area and perimeter of a rectangle given its length and width. It does the job, but it's repetitive and a bit messy.

```python
# Original Version
length = 4
width = 3

# Calculate area and perimeter
area = length * width
perimeter = 2 * (length + width)

print("Area:", area)
print("Perimeter:", perimeter)
```

We can refactor this by encapsulating the functionality into functions, which makes the code more organized and reusable:

```python
# Refactored Version

def calculate_area(length, width):
    return length * width

def calculate_perimeter(length, width):
    return 2 * (length + width)

# usage
length = 4
width = 3

print("Area:", calculate_area(length, width))
print("Perimeter:", calculate_perimeter(length, width))
```

Both snippets output the same result:
```
Area: 12
Perimeter: 14
```

But the refactored version is cleaner and separates concerns, making it easier to update one calculation without affecting the other.

## Deep Dive
Refactoring has its roots in the early days of software engineering when programmers realized that code could—and should—be improved even if it is already "working". Martin Fowler's seminal book "Refactoring: Improving the Design of Existing Code" articulated many core principles and techniques. He famously said, "Any fool can write code that a computer can understand. Good programmers write code that humans can understand."

Alternatives to refactoring might include rewriting code from scratch or making minor tweaks without systematic improvement. However, refactoring is usually more cost-effective than a rewrite and less risky than ad-hoc modifications. Implementation details can be specific to each programming paradigm; however, object-oriented programming lends itself particularly well to refactoring, especially with techniques like extracting methods (like our `calculate_area` and `calculate_perimeter` functions), inlining, moving features between objects, and renaming methods or variables for clarity.

Refactoring in Python often uses tools like `PyCharm`, which has built-in refactoring capabilities, or `rope`, a Python library specifically designed for refactoring. Careful use of version control, such as `git`, during refactoring is strongly advised to keep track of changes incrementally.

## See Also
For those hungry for more, dive into the following resources:
- Martin Fowler's book: [Refactoring: Improving the Design of Existing Code](http://www.refactoring.com/)
- Python refactoring with `rope`: [GitHub - rope](https://github.com/python-rope/rope)
- PyCharm refactoring documentation: [Jetbrains PyCharm Refactoring Source Code](https://www.jetbrains.com/help/pycharm/refactoring-source-code.html)
- Refactoring.guru: [Refactoring and Design Patterns](https://refactoring.guru/refactoring)
- Clean Code lectures by Uncle Bob (Robert C. Martin): [Clean Code - Uncle Bob / Lesson 1](https://www.youtube.com/watch?v=7EmboKQH8lM)
