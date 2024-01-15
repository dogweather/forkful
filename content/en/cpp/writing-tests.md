---
title:                "Writing tests"
html_title:           "C++ recipe: Writing tests"
simple_title:         "Writing tests"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/writing-tests.md"
---

{{< edit_this_page >}}

## Why

Testing is an essential part of the software development process, allowing developers to catch bugs and errors early on, which can save time and resources later on. Writing tests not only ensures that the code is functioning correctly but also acts as documentation for future changes and updates.

## How To

To write tests in C++, you will need a testing framework such as Catch2 or Google Test. These frameworks provide useful macros and functions for writing and running tests. Here is an example of how to write a simple test using Catch2:

```
#include <iostream>
#define CATCH_CONFIG_MAIN
#include "catch.hpp"

// Function to be tested
int addition(int a, int b) {
    return a + b;
}

TEST_CASE("Testing addition function", "[addition]") {
    REQUIRE(addition(2, 3) == 5);
}
```

In the above code, we have included the header file for Catch2, defined our main function, and written a simple test case using the `REQUIRE` macro, which checks if the result of the `addition` function is equal to 5.

To run this test, we need to compile and link it with the Catch2 library, and then execute the resulting executable. Once the test is run, we will get an output like this:

```
===============================================================================
All tests passed (1 assertion in 1 test case)
```

This output indicates that our test was successful. In case the test fails, the output will show which assertion failed, making it easier to identify and fix the issue.

## Deep Dive

Writing good tests involves more than just checking if the code is producing the correct output. It's also crucial to cover different scenarios and edge cases to ensure the stability and robustness of the code.

Apart from the traditional unit tests, there are also other types of tests such as integration tests and system tests, which can help detect errors in the interactions between different components of the software. It's essential to have a good mix of these different types of tests to have thorough coverage.

Moreover, it's also crucial to continuously update and maintain the tests as the codebase evolves. This helps in catching bugs and regressions early on, saving time and resources in the long run.

## See Also

- [Catch2 homepage](https://github.com/catchorg/Catch2)
- [Google Test](https://github.com/google/googletest)
- [Testing Your Code in C++](https://www.ibm.com/docs/en/zos/icc?topic=compiling-testing-code-cpp)