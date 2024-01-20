---
title:                "Writing tests"
html_title:           "Arduino recipe: Writing tests"
simple_title:         "Writing tests"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?

Writing tests means crafting code that checks if your other code works. Programmers do it to catch bugs early, save time, and make sure that code works now and keeps working later.

## How to:

In C, you can write tests by using a testing framework like CUnit or by hand-rolling your own simple testing setup. Below is a barebones example using assert to create a basic test function for an `add` function.

```C
#include <assert.h>

// Function to test
int add(int a, int b) {
    return a + b;
}

// Test function
void test_add() {
    assert(add(2, 2) == 4);
    assert(add(-1, 1) == 0);
}

int main() {
    test_add();
    printf("All tests passed!\n");
    return 0;
}
```

Sample output, if all tests pass:

```
All tests passed!
```

If a test fails, the program will abort and print an error message.

## Deep Dive

Historically, C didn't come with a built-in testing framework. Programmers usually wrote custom test functions or used third-party frameworks. Popular frameworks include CUnit, Check, and Unity. Each offers features like automated test discovery, setup/teardown processes, and test result reporting. For small projects, simple assert-based tests might suffice, but as complexity grows, a proper framework saves time and hassle.

## See Also

Here are some useful links for more immersive diving:

- [CUnit](http://cunit.sourceforge.net/)
- [Check](https://libcheck.github.io/check/)
- [Unity](http://www.throwtheswitch.org/unity)
- [Assert.h in C programming](https://www.tutorialspoint.com/assert-h-in-c-programming)