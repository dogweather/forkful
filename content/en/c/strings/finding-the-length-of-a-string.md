---
date: 2024-02-03 17:49:55.005016-07:00
description: 'How to: In C, the standard library function `strlen()` is commonly used
  to find the length of a string. Here''s a quick example.'
lastmod: '2024-03-13T22:45:00.503541-06:00'
model: gpt-4-0125-preview
summary: In C, the standard library function `strlen()` is commonly used to find the
  length of a string.
title: Finding the length of a string
weight: 7
---

## How to:
In C, the standard library function `strlen()` is commonly used to find the length of a string. Here's a quick example:

```c
#include <stdio.h>
#include <string.h>

int main() {
    char myString[] = "Hello, World!";
    size_t length = strlen(myString);
    
    printf("Length of '%s' is %zu.\n", myString, length);
    
    return 0;
}
```

**Sample Output:**
```
Length of 'Hello, World!' is 13.
```

In this example, `strlen()` takes a string (`myString`) as input and returns its length excluding the null terminator. The use of `size_t` for the length variable is recommended because it is an unsigned integer type, making it capable of representing the size of the largest possible object on the system.

## Deep Dive:
The `strlen()` function has been a part of the C standard library since the language's inception. Under the hood, it works by incrementing a counter as it traverses the string until it hits the null terminator. This simplicity, however, comes with performance considerations: because `strlen()` counts characters at runtime, repeatedly calling it on the same string in a loop, for instance, is inefficient.

In terms of security, `strlen()` and other C string-handling functions do not inherently check for buffer overruns, making careful programming essential to avoid vulnerabilities. Modern alternatives in other languages, such as string types that include the length or use safe buffer handling by default, eliminate some of these risks and inefficiencies.

Despite its limitations, understanding `strlen()` and manual string handling in C is crucial for programmers, especially when working with low-level code or when performance and memory control are paramount. It also offers valuable insights into the workings of higher-level string abstractions in other languages.
