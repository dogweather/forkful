---
title:                "Writing tests"
html_title:           "C recipe: Writing tests"
simple_title:         "Writing tests"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?

Writing tests in C is the process of creating small code snippets or functions that test various aspects of your program. These tests help ensure that your code is functioning as intended and can catch any potential bugs or errors before they become larger issues. Programmers write tests to increase the reliability and quality of their code, ultimately making it easier to maintain and debug.

## How to:

To create a test in C, you first need to declare a `main` function, just like you would for any other C program. Within this function, you can use the `assert` function from the `<assert.h>` header file to check if a certain condition is true. Here's an example:

```C
#include <assert.h>

int main() {
    int num = 5;
    assert(num < 10);  // this test will pass
    assert(num == 10); // this test will fail
    return 0;
}
```

You can also use the `printf` function to display any desired output. To run your tests, compile and execute your program, and check the output to see if all tests passed or if any failed.

## Deep Dive:

In the early days of C programming, writing tests was not a common practice due to the limitations of older testing frameworks. However, with the advancement of technology and the creation of new testing libraries, writing tests in C has become more accessible and popular.

An alternative to using the `assert` function is to use a testing library such as `cmocka` or `seatest`. These libraries provide additional features and testing capabilities, making it easier to write and manage tests. They also allow for more detailed output and enable test-driven development, where tests are written before the actual code.

When writing tests, it's essential to have a good understanding of your code's logic and behavior. This helps in creating effective tests and improves the overall quality of your code. Additionally, it's crucial to regularly run your tests and make updates as your codebase changes.

## See Also:

- [Official C documentation on <assert.h>](https://en.cppreference.com/w/c/error/assert)
- [cmocka testing library](https://cmocka.org/)
- [seatest testing library](https://github.com/phil-wilson/seatest)