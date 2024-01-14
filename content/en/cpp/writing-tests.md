---
title:    "C++ recipe: Writing tests"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Why

Writing tests may not seem like the most exciting part of coding, but it is an essential aspect of ensuring the quality and functionality of your code. By writing tests, you can catch and fix bugs early on, save time and resources in the long run, and have more confidence in your code.

## How To

To write tests in C++, you will need to use a testing framework such as Google Test or Catch2. These frameworks provide a set of macros and functions that allow you to write and run tests easily. Let's take a look at a simple example using Google Test:

```C++
#include <gtest/gtest.h>

// The function we want to test
int square(int num) {
    return num * num;
}

// A test case that checks if the square function returns the correct result for a given input
TEST(SquareTest, PositiveNumber) {
    // Arrange
    int num = 5;
    int expected = 25;

    // Act
    int result = square(num);

    // Assert
    EXPECT_EQ(expected, result);
}

// Run all the tests defined
int main(int argc, char** argv) {
    testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
```

In this example, we have defined a test case using the `TEST` macro, which takes two parameters: the name of the test case (in this case, "SquareTest") and the name of the test (in this case, "PositiveNumber"). Within the test case, we have arranged our input, calling the function we want to test, and asserting that the result is equal to our expected output using the `EXPECT_EQ` macro. Finally, in our `main` function, we initialize the testing framework and run all the tests defined.

When running this code, we should see the following output:

```
[==========] Running 1 test from 1 test suite.
[----------] Global test environment set-up.
[----------] 1 test from SquareTest
[ RUN      ] SquareTest.PositiveNumber
[       OK ] SquareTest.PositiveNumber (0 ms total)
[----------] 1 test from SquareTest (0 ms total)

[----------] Global test environment tear-down
[==========] 1 test from 1 test suite ran. (4 ms total)
[  PASSED  ] 1 test.
```

## Deep Dive

Now, let's dive a bit deeper into the benefits of writing tests. As mentioned earlier, tests help catch and fix bugs early on, which can save a lot of time and resources. Imagine if you had written a complex function without any tests and later discovered that it had a bug. It could potentially take hours or even days to figure out and fix the issue, while a simple test could have caught it in a matter of minutes.

Moreover, writing tests also allows for easier code refactoring. When making changes to your code, you can run your tests to ensure that the functionality hasn't been affected. If any tests fail, you know that something in your code needs to be fixed. This helps prevent unexpected bugs from popping up in the future.

Additionally, writing tests forces you to think about the functionality of your code and the potential edge cases. By covering these cases in your tests, you are ultimately improving the overall quality of your code.

## See Also

- [Google Test Documentation](https://github.com/google/googletest/blob/master/googletest/docs/Primer.md)
- [Catch2 Tutorial](https://github.com/catchorg/Catch2/blob/master/docs/tutorial.md)
- [Test-Driven Development](https://en.wikipedia.org/wiki/Test-driven_development)