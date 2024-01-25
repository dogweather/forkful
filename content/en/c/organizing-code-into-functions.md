---
title:                "Organizing code into functions"
date:                  2024-01-25T02:59:38.253554-07:00
model:                 gpt-4-1106-preview
simple_title:         "Organizing code into functions"
programming_language: "C"
category:             "C"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## What & Why?
Organizing code into functions is about breaking down the code into reusable blocks that perform specific tasks. It makes code easier to read, debug, and maintain.

## How to:
Let's take a simple example: say, you want to add two numbers multiple times.

Without functions:
```C
#include <stdio.h>

int main() {
    int sum1 = 5 + 3;
    printf("Sum1: %d\n", sum1);
    
    int sum2 = 2 + 8;
    printf("Sum2: %d\n", sum2);
    
    // More additions here...
    
    return 0;
}
```

With functions:
```C
#include <stdio.h>

int add(int a, int b) {
    return a + b;
}

int main() {
    int sum1 = add(5, 3);
    printf("Sum1: %d\n", sum1);
    
    int sum2 = add(2, 8);
    printf("Sum2: %d\n", sum2);
    
    // Use add() function for more additions...
    
    return 0;
}
```

Output:
```
Sum1: 8
Sum2: 10
```

## Deep Dive
Before C had functions, programming was often done in a linear fashion, much like a recipe. But as programs grew, code duplication became a problem. Functions were the solution - they allowed us to execute the same code block from different parts of a program without rewriting it every time. This not only saves space but also time when making updates: change the function in one place, and every part of your code that uses it gets the update.

Alternatives to functions might include inline code, macros, or copy-and-paste coding, but these can lead to bloated, error-prone, and hard-to-maintain code. Functions, by contrast, encapsulate functionality, define clear interfaces, and can reduce side effects with proper use of scope.

When you're implementing functions, consider a couple of details: one, try to make them do just one thing – this is known as the Single Responsibility Principle. Two, names are important – choose descriptive names for functions and their parameters to make your code self-documenting.

## See Also
For more on functions in C, take a gander at these:

- C Standard Library reference: https://en.cppreference.com/w/c/header
- C Programming: A Modern Approach by K.N. King: A book with a deep dive on functions.
- Learn-C.org: Functions section: https://www.learn-c.org/en/Functions