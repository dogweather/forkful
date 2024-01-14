---
title:    "C recipe: Writing tests"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/c/writing-tests.md"
---

{{< edit_this_page >}}

## Why
Writing tests may seem like a daunting task, especially for new programmers. But trust me, it's worth the effort. Testing your code not only helps ensure that your program is functioning correctly, it also saves you time and headaches in the long run.

## How To
To write tests in C, all you need is a good understanding of the language, a compiler, and a testing framework. Let's take a look at an example using the popular CUnit testing framework.

First, we need to include the necessary headers and define our test function:
```C
#include <CUnit/CUnit.h> // include CUnit header
#include <CUnit/Basic.h> // include CUnit basic mode header

void test_function(void)
{
    // test code goes here
}
```
Next, we need to initialize the CUnit framework and register our test function:
```C
int main(void)
{
    // initialize CUnit
    CU_initialize_registry();

    // register the test function
    CU_pSuite suite = CU_add_suite("Test Example", NULL, NULL);
    CU_add_test(suite, "test_func", test_function);

    // run all tests
    CU_basic_run_tests();

    // cleanup CUnit
    CU_cleanup_registry();

    return 0;
}
```
Finally, we write our test code within the test function and use CUnit's assertion functions to check for expected results:
```C
void test_function(void)
{
    // test code
    int result = 2 + 2;

    // assert expected result
    CU_ASSERT_EQUAL(result, 4); // this test will pass

    // another test
    char str[10];
    strcpy(str, "Hello World!");

    // assert expected result
    CU_ASSERT_STRING_EQUAL(str, "Hello World!"); // this test will also pass
}
```
After running our program, we get an output like this:
```bash
CUnit - A Unit testing framework for C - Version 2.1-3
http://cunit.sourceforge.net/

Suite: Test Example
  Test name: test_func...passed
  Test name: test_func2...passed

Run Summary:    Type  Total    Ran Passed Failed Inactive
              suites      1      1    n/a      0        0
               tests      2      2      2      0        0
             asserts      2      2      2      0      n/a

Elapsed time =    0.000 seconds
```

## Deep Dive
Writing effective tests involves understanding different types of testing, such as unit testing and integration testing, and knowing how to write tests that cover all possible scenarios.

Unit testing involves testing individual units or components of your code in isolation to ensure they behave as expected. This can be achieved using techniques like stubs and mocking, which simulate the behavior of external dependencies.

Integration testing involves testing how different units work together to ensure the overall functionality of your code is correct. This type of testing is more complex and usually requires a testing framework that can handle multiple units.

When writing tests, it is important to consider both positive and negative test cases to cover all possible scenarios and catch any potential errors. It is also helpful to write tests alongside your code as you develop, so that any bugs can be caught and fixed early on.

## See Also
- [CUnit official documentation](http://cunit.sourceforge.net/)
- [Introduction to Unit Testing in C using CUnit](https://www.youtube.com/watch?v=mZZ-jJcJ2yU)
- [Writing a Simple CUnit Test in C](https://eromoe.com/til/c/cunit-intro-01/)