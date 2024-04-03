---
date: 2024-02-03 19:03:20.795391-07:00
description: 'How to: #.'
lastmod: '2024-03-13T22:45:00.360726-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: Writing tests
weight: 36
---

## How to:


### Using Google Test Framework
One of the most popular third-party libraries for writing tests in C++ is Google Test. First, you'll need to install Google Test and link it with your project. Once set up, you can start writing test cases.

```cpp
#include <gtest/gtest.h>

int add(int a, int b) {
    return a + b;
}

TEST(TestSuiteName, TestName) {
    EXPECT_EQ(3, add(1, 2));
}

int main(int argc, char **argv) {
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
```

Save the code in a file, and compile it with the g++ compiler, linking the Google Test library. If everything is set up correctly, running the resulting executable will run the test, and if the `add` function works as expected, you'll see something like:

```
[==========] Running 1 test from 1 test suite.
[----------] Global test environment set-up.
[----------] 1 test from TestSuiteName
[ RUN      ] TestSuiteName.TestName
[       OK ] TestSuiteName.TestName (0 ms)
[----------] 1 test from TestSuiteName (0 ms total)

[==========] 1 test from 1 test suite ran. (1 ms total)
[  PASSED  ] 1 test.
```

### Using Catch2
Another popular testing framework for C++ is Catch2. It has a simpler syntax and doesn't usually require linking against a library (header-only). Here's an example of how to write a simple test with Catch2:

```cpp
#define CATCH_CONFIG_MAIN  // This tells Catch to provide a main() - only do this in one cpp file
#include <catch.hpp>

int multiply(int a, int b) {
    return a * b;
}

TEST_CASE( "Integers are multiplied", "[multiply]" ) {
    REQUIRE( multiply(2, 3) == 6 );
}
```

Upon compiling and running this test, Catch2 provides clear output indicating whether the test passed or failed, along with any information needed to debug failures:

```
===============================================================================
All tests passed (1 assertion in 1 test case)
```

These examples show how integrating testing frameworks into your C++ development workflow can significantly enhance the reliability and maintainability of your code.
