---
title:                "C++ recipe: Writing tests"
simple_title:         "Writing tests"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/writing-tests.md"
---

{{< edit_this_page >}}

## Why Engage in Writing Tests

Writing tests for your code may seem like an extra and unnecessary step in the development process. However, it is actually an essential component of writing high quality, error-free code. By writing tests, you can catch bugs and issues early on, saving yourself time and headaches in the future.

## How To Write Tests in C++

In order to write tests in C++, you will need to use a testing framework, such as Google Test or Catch2. These frameworks provide useful functions and macros for creating and running tests. Let's take a look at an example using Google Test:

```C++
#include <gtest/gtest.h> //include Google Test framework

//create a test case
TEST(AdditionTest, SimpleAddition) {

    //define the variables to test
    int num1 = 5;
    int num2 = 10;

    //perform the operation and store the result
    int result = num1 + num2;

    //write an assertion to check if the result is correct
    EXPECT_EQ(result, 15);
}

//run all the tests in the code
int main(int argc, char** argv) {
    testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
```

In the above example, we have defined a test case called "SimpleAddition" which checks if the result of adding two numbers is correct. We use the `TEST()` macro to create the test case and the `EXPECT_EQ()` macro to write our assertion. Finally, in the `main()` function, we use Google Test's `RUN_ALL_TESTS()` function to run all the tests in our code.

## Deep Dive into Writing Tests

Writing tests not only helps catch bugs, but it also promotes good coding practices. By writing tests, you are forced to think about different scenarios and edge cases that your code may encounter, leading to more thorough and robust code. Writing tests also allows for easier debugging, as you can pinpoint the exact source of an issue through a failed test.

It is important to note that writing tests is not a substitute for thorough code reviews and proper debugging techniques. It should be used in conjunction with these practices to ensure the highest quality code.

## See Also

- [Google Test documentation](https://github.com/google/googletest)
- [Catch2 documentation](https://github.com/catchorg/Catch2)
- [Tutorial on writing tests in C++](https://www.learncpp.com/cpp-tutorial/89-unit-testing-with-google-test-and-google-mock/)