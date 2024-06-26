---
date: 2024-01-25 02:59:34.954384-07:00
description: "How to: Here's a classic example \u2014 a function to calculate the\
  \ factorial of a number."
lastmod: '2024-03-13T22:44:59.977282-06:00'
model: gpt-4-1106-preview
summary: "Here's a classic example \u2014 a function to calculate the factorial of\
  \ a number."
title: Organizing code into functions
weight: 18
---

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
