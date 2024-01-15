---
title:                "כתיבת בדיקות"
html_title:           "Haskell: כתיבת בדיקות"
simple_title:         "כתיבת בדיקות"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## Why (למה)

Programming languages have evolved, and with them, the need for writing robust and error-free code has become more important than ever. Testing is an essential aspect of software development, ensuring that the code performs as expected and catches any bugs or errors before it reaches the end-users. In Haskell, a functional programming language, writing tests can greatly improve the quality and reliability of your code.

## How To (כיצד)

Writing tests in Haskell is relatively easy and straightforward. In this section, we will cover some coding examples and sample output to get you started.

First, let's import the `Test.HUnit` module, which is an assertion library used for writing unit tests in Haskell.

```Haskell
import Test.HUnit
```

Next, let's define a simple function that adds two numbers together.

```Haskell
add :: Int -> Int -> Int
add x y = x + y
```

Now, we can write a test case for this function using the `assertEqual` function from the `Test.HUnit` module. The `assertEqual` function takes in two values and compares them, throwing an error if they are not equal. In this case, we are asserting that the result of `add 2 3` is equal to 5.

```Haskell
testAddition = TestCase (assertEqual "Adding two numbers" 5 (add 2 3))
```

Finally, we can run this test and see its output in the terminal.

```
$ runTestTT testAddition
Cases: 1  Tried: 1  Errors: 0  Failures: 0
Cases: 1  Tried: 1  Errors: 0  Failures: 0
Counts {cases = 1, tried = 1, errors = 0, failures = 0}
```

This tells us that the test passed, and our `add` function is working correctly.

## Deep Dive (עומק)

There are various types of tests that you can write in Haskell, including unit tests, integration tests, and property tests. Unit tests, as shown in the previous example, are used to test individual functions or modules. Integration tests are used to test the interaction between different parts of the code. Property tests, on the other hand, use randomly generated input data to test a property or invariant of a function.

In Haskell, there are many tools available for writing tests, such as `QuickCheck` and `HSpec`. These tools provide additional features, such as automatically generating test cases and defining test suites, making it easier to write and maintain tests.

Remember, writing tests is not just about ensuring the correctness of your code but also improves overall code quality, making it easier to maintain and debug in the future.

## See Also (ראה גם)

- [Hackage - Test.HUnit module](https://hackage.haskell.org/package/HUnit-1.6.0.0/docs/Test-HUnit.html)
- [Hoogle - QuickCheck module](https://hackage.haskell.org/package/QuickCheck-2.14/docs/Test-QuickCheck.html)
- [HSpec - User guide](https://hspec.github.io/)