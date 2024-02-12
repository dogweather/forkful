---
title:                "Writing tests"
aliases: - /en/c/writing-tests.md
date:                  2024-02-03T17:49:58.148338-07:00
model:                 gpt-4-0125-preview
simple_title:         "Writing tests"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
Writing tests in C involves creating smaller, auxiliary programs or functions that automatically verify the functionality of your code. Programmers do it to ensure their software works as expected, to catch bugs early, and to facilitate future code modifications without unintended side effects.

## How to:
While C doesn't have a built-in testing framework like some other languages, you can still write effective tests using assert.h for simple assertions or integrate third-party frameworks like CUnit or Unity for more structured testing. Here's a basic example using assert.h to test a function that adds two integers:

```c
#include <assert.h>
#include "my_math.h"

void test_addition() {
    assert(add(1, 2) == 3);
    assert(add(-1, -2) == -3);
    assert(add(0, 0) == 0);
    printf("All addition tests passed.\n");
}

int main() {
    test_addition();
    return 0;
}
```

In `my_math.h`, you might have:

```c
// Simple addition function
int add(int a, int b) {
    return a + b;
}
```

Running the test function in your `main` function outputs:

```
All addition tests passed.
```

For a more comprehensive testing setup using a framework like Unity, you would incorporate the framework into your project, then write test cases similarly, but utilizing the framework's API for assertions and test running.

## Deep Dive
Testing in C has historically been a manual and somewhat ad hoc process due to the language's low-level nature and the lack of a standardized testing framework. This manual approach often led to less thorough testing practices compared to languages with built-in testing support. As the C language has been crucial in the development of foundational software systems, this lack of formal testing frameworks prompted the C community to develop third-party solutions, like CUnit and Unity.

These tools, while external to the standard C library, provide functionality akin to testing frameworks in other languages, offering a structured way to define, run, and evaluate tests. They help bridge the gap between C's powerful system-level access and the modern development practice of automated testing. It's worth noting that while these tools greatly enhance the testing process in C, they can introduce a learning curve and increase the complexity of project setup compared to languages with integrated testing support. Thus, for projects where reliability and maintainability are paramount, the investment in setting up a proper testing environment in C is well justified, even in light of possible alternatives.
