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

## What & Why?
Writing tests is the process of creating code that checks if your program is working as intended. It involves writing small pieces of code that test specific parts of your program's functionality. Programmers do this to ensure that their code is functioning correctly and to catch any bugs that may arise.

## How to:
Writing tests in C++ can be done using a library called "Catch2." This library provides an easy-to-use framework for creating and running tests. Here's an example of a test using Catch2:

```C++
TEST_CASE("Adding Numbers") {
  REQUIRE(add(2, 3) == 5);
}
```

The test above checks if the `add` function correctly adds two numbers and returns the expected result. If the test fails, Catch2 will provide information on what went wrong. Here's a sample output:

```
===============================================================================
Adding Numbers
===============================================================================
test.cpp:3
...............................................................................

test.cpp:5: FAILED:
  REQUIRE( add(2, 3) == 4 )
with expansion:
  5 == 4
===============================================================================
test cases: 1 | 1 failed
checksum: 3a4e471943a48dd1f014dd1c6a1271aa778da841
```

This output indicates that the test failed because the `add` function did not return the expected result. By writing tests, we can catch these types of errors and fix them before the code reaches the production stage.

## Deep Dive:
Writing tests has become an essential practice in software development. It originated with the emergence of Test-Driven Development (TDD) in the early 2000s. TDD is a development process that involves writing tests before writing the actual code. This approach helps to ensure that the code meets the desired functionality and catches any bugs early on.

An alternative to using Catch2 is Google Test, a testing framework developed by Google. It shares many similarities with Catch2, but some developers prefer Google Test for its extensive documentation and support for different programming languages.

Writing tests also allows developers to continuously run and monitor their code's functionality, known as Continuous Integration. This practice ensures that any changes to the code do not break its existing functionality.

## See Also:
- [Catch2 Official Documentation](https://github.com/catchorg/Catch2/blob/master/docs/Readme.md)
- [Google Test Official Documentation](https://github.com/google/googletest/blob/master/googletest/docs/primer.md)
- [Test-Driven Development Explained](https://www.agilealliance.org/glossary/tdd/)