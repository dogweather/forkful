---
title:                "C recipe: Writing tests"
simple_title:         "Writing tests"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/writing-tests.md"
---

{{< edit_this_page >}}

## Why
Writing tests may seem like an extra step in the coding process, but trust us, it's worth it. Testing your code ensures that it functions correctly and efficiently, saving you time and effort in the long run. Plus, it helps catch any bugs or errors early on, preventing them from becoming bigger issues down the line.

## How To
To start writing tests in C, you'll need a testing framework. One popular option is cmocka, a lightweight testing framework for C. Here's an example of how to use cmocka to test a simple function:

```
#include <stdio.h>
#include <cmocka.h>

// Function to test
int add(int a, int b) {
    return a + b;
}

// Test case 1
static void test_add(void **state) {
    assert_int_equal(add(2, 3), 5);
}

// Test case 2
static void test_add_negative(void **state) {
    assert_int_equal(add(-2, -3), -5);
}

int main() {
    // Create an array of test cases
    const struct CMUnitTest tests[] = {
        cmocka_unit_test(test_add),
        cmocka_unit_test(test_add_negative),
    };

    // Run all tests
    return cmocka_run_group_tests(tests, NULL, NULL);
}
```

The test cases are defined using `cmocka_unit_test` and each one contains an assertion to check the result of the function. If all assertions pass, the test is considered successful.

Running this code produces the following output:

```
[==========] Running 2 test case(s).
[----------] Global test environment set-up.
[----------] 1 test(s) run.
[ RUN      ] test_add
[       OK ] test_add
[ RUN      ] test_add_negative
[       OK ] test_add_negative
[----------] 2 test(s) run.
[----------] Global test environment tear-down.
[==========] 2 test(s) run.
[  PASSED  ] 2 test(s).
```

## Deep Dive
When it comes to writing tests, it's important to think about various scenarios and edge cases to thoroughly test your code. Make sure to cover both expected and unexpected inputs to ensure your code can handle all possible scenarios.

It's also a good practice to follow the Arrange-Act-Assert (AAA) pattern when writing test cases. This means clearly separating the setup, execution, and assertion parts of your test to make it more readable and maintainable.

Additionally, don't forget to regularly update your tests as you make changes to your code. This helps catch any new bugs that may arise and ensures your code continues to function correctly.

## See Also
- [cmocka](https://cmocka.org/)
- [Unit Testing in C using cmocka](https://www.oreilly.com/content/unit-testing-in-c-using-cmocka/)
- [Writing Testable Code in C](https://philsquared.co.uk/blog/2019/05/25/writing-testable-code-in-c/)