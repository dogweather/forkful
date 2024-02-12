---
title:                "Organizing code into functions"
aliases:
- en/java/organizing-code-into-functions.md
date:                  2024-01-25T02:59:34.954384-07:00
model:                 gpt-4-1106-preview
simple_title:         "Organizing code into functions"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## What & Why?
Organizing code into functions means breaking down the beast of a program into manageable chunks, each doing a distinct task. Programmers do this to make code readable, reusable, and maintainable.

## How to:
Here's a classic example — a function to calculate the factorial of a number.

```java
public class MathUtils {

    public static void main(String[] args) {
        int number = 5;
        int result = factorial(number);
        System.out.println("Factorial of " + number + " is: " + result);
    }
    
    public static int factorial(int n) {
        if (n <= 1) {
            return 1;
        }
        return n * factorial(n - 1);
    }
}
```

Output would be:
```
Factorial of 5 is: 120
```

## Deep Dive
Before functions were a thing, code was crammed into monolithic blocks, making debugging like finding a needle in a haystack. Now, encapsulating functionality into functions helps isolate issues fast. Alternatives include lambda expressions in Java or methods in object-oriented programming, both serving similar purposes. When you write a function, remember: (1) Each function should have a single responsibility and (2) the name of the function should clearly describe its purpose.

## See Also
For more on organizing code:
- Clean Code by Robert C. Martin
- Refactoring: Improving the Design of Existing Code by Martin Fowler
- [Oracle Java docs on Defining Methods](https://docs.oracle.com/javase/tutorial/java/javaOO/methods.html)
