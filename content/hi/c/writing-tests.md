---
title:                "परीक्षाएं लिखना"
html_title:           "C: परीक्षाएं लिखना"
simple_title:         "परीक्षाएं लिखना"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/writing-tests.md"
---

{{< edit_this_page >}}

## Why

Tests are an essential part of any programming project, including C. They help ensure that our code works as intended, identify bugs early on, and make it easier to maintain and troubleshoot our code in the long run. In other words, writing tests can save us a lot of time and headaches in the future.

## How To

```C
#include<stdio.h>

// A function that adds two numbers
int add(int num1, int num2) {
    return num1 + num2;
}

int main() {
    // Testing the add function
    int result = add(3, 7);
    if (result == 10) {
        printf("Test Passed!");
    } else {
        printf("Test Failed.");
    }
}
```

When writing tests, we want to cover different scenarios and edge cases to ensure our code is robust. This is done by setting up input values and comparing the output with our expected result. If they match, our test passes, and if not, our test fails. With this approach, we can catch bugs early on and have confidence in our code.

## Deep Dive

While writing tests may seem like extra work, it can actually save us time in the long run. They serve as living documentation, providing us with examples of how our code should be used. This is useful for future maintenance or when collaborating with others on the project. Additionally, some tools like Valgrind can help us catch memory leaks and other errors that may not be immediately noticeable.

## See Also

- [TDD in C: Getting Started](https://prakhar.me/articles/tdd-in-c/#)
- [Unit Testing in C](https://www.thecrazyprogrammer.com/2018/03/unit-testing-c-programming-language.html)
- [Using Valgrind for C Debugging](https://medium.com/@christinasc/using-valgrind-for-c-debugging-b01e9832da4a)