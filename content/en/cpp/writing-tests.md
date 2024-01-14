---
title:    "C++ recipe: Writing tests"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/cpp/writing-tests.md"
---

{{< edit_this_page >}}

## Why Writing Tests is Important in C++

Writing code is just one aspect of creating a robust and reliable software. However, ensuring that the code functions as expected under different scenarios is equally important. And that's where writing tests comes into play.

Writing tests allows developers to catch bugs and errors early on in the development process, reducing the chances of releasing a flawed product. It also helps in maintaining and enhancing the codebase, making it easier to identify and fix issues as the codebase grows.

## How To Write Tests in C++

In C++, the most common testing framework used is Google Test. It provides a simple and intuitive way to write tests.

To get started with writing tests in C++, you first need to include the Google Test framework in your project. Once that is done, you can write your test cases using `TEST()` macro. Here's an example of a simple test case:

```C++
#include <gtest/gtest.h>

TEST(SampleTest, Addition) {
    int result = 1 + 2;
    ASSERT_EQ(result, 3);
}
```

In the above example, we have defined a test case named "SampleTest" which checks the addition of two numbers.

To run the tests, you need to compile and execute the test file. Google Test will then provide you with the output of the test cases, indicating whether they passed or failed.

## Deep Dive into Writing Tests

Tests can also be divided into test fixtures and test suites. A test fixture is a piece of code that sets up the environment for multiple test cases, and a test suite is a collection of test cases.

Google Test provides support for test fixtures and suites using `TEST_F()` and `TEST_SUITE()` macros, respectively. Here's an example of how they can be used:

```C++
#include <gtest/gtest.h>

// Defining a test fixture
class SampleTestFixture : public testing::Test {
protected:
    int x = 5;
    int y = 10;
};

// Defining a test suite with multiple test cases
TEST_F(SampleTestFixture, Addition) {
    int result = x + y;
    ASSERT_EQ(result, 15);
}

TEST_F(SampleTestFixture, Subtraction) {
    int result = y - x;
    ASSERT_EQ(result, 5);
}

// Defining multiple test suites
TEST_SUITE(AnotherTestSuite);

TEST(AnotherTestSuite, Multiplication) {
    int result = x * y;
    ASSERT_EQ(result, 50);
}
```

Apart from these, there are various other features that Google Test offers, such as parameterized tests, death tests, and type-parameterized tests. It's worth exploring these features for more sophisticated testing.

## See Also

- Google Test [documentation](https://github.com/google/googletest)
- C++Unit [framework](https://github.com/cppunit/cppunit)
- Test Driven Development [guide](https://www.agilealliance.org/glossary/test-driven-development/)

By writing tests, you not only improve the quality of your code but also gain confidence in its functionality. With testing, you can catch issues early on and ensure that your code performs as expected. So, next time you write code in C++, don't forget to write tests for it!