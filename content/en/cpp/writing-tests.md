---
title:                "C++ recipe: Writing tests"
programming_language: "C++"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/writing-tests.md"
---

{{< edit_this_page >}}

## Why Writing Tests Is Important in C++

Writing tests is an essential part of the software development process. It allows developers to catch bugs and errors early on, which can save time and resources in the long run. Additionally, writing tests can provide a sense of security and confidence in the code, as it has been thoroughly tested.

## How To Write Tests in C++

Writing tests in C++ can be done using various testing frameworks such as Google Test, Catch2, or Boost.Test. Here is a simple example using Google Test:

```C++
#include <iostream>
#include <gtest/gtest.h> //include the Google Test library

int sum(int a, int b)
{
	return a + b;
}

TEST(SumTest, PositiveNumbers)
{
	EXPECT_EQ(sum(5, 6), 11); //test if the sum of 5 and 6 is 11
}

int main(int argc, char* argv[])
{
	testing::InitGoogleTest(&argc, argv); //initialize Google Test
	return RUN_ALL_TESTS(); //run all tests
}
```

Running this code will produce the following output:

```
[==========] Running 1 test from 1 test suite.
[----------] Global test environment set-up.
[----------] 1 test from SumTest
[ RUN      ] SumTest.PositiveNumbers
[       OK ] SumTest.PositiveNumbers (0 ms total)
[----------] 1 test from SumTest (0 ms total)

[----------] Global test environment tear-down
[==========] 1 test from 1 test suite ran. (0 ms total)
[  PASSED  ] 1 test.
```

The `TEST` macro allows us to create different test cases within the same test suite. In this example, we have one test case called `PositiveNumbers`, where we use the `EXPECT_EQ` assertion to test if the sum of two positive numbers is correct. There are many other types of assertions available in Google Test, depending on the type of test being performed.

## Deep Dive into Writing Tests

Writing effective tests requires careful consideration and planning. Some tips to keep in mind when writing tests in C++ include:

- Test the boundary cases: Make sure to test not only the regular cases but also the edge cases, such as empty inputs, negative numbers, etc. This can help uncover potential bugs that may not have been caught otherwise.
- Keep the tests short and simple: Each test should focus on a specific scenario and be easy to understand. This helps with debugging and troubleshooting.
- Use meaningful test names: This can help identify the test and its purpose when reviewing the test results.
- Test only one thing at a time: Avoid testing multiple functions or scenarios in the same test, as it can make it harder to pinpoint the source of a failed test.
- Regularly update and maintain tests: As the codebase evolves, so should the tests. Make sure to update tests when new features are added or existing ones are modified.

Overall, writing tests takes time and effort, but it is crucial for ensuring the quality and reliability of the code.

## See Also

To learn more about writing tests in C++, check out these resources:

- [Google Test Documentation](https://github.com/google/googletest/blob/master/googletest/docs/Primer.md)
- [Catch2 Tutorial](https://github.com/catchorg/Catch2/blob/master/docs/tutorial.md)
- [Boost.Test Tutorial](https://www.boost.org/doc/libs/1_75_0/libs/test/doc/html/boost_test/adv_scenarios/index.html)