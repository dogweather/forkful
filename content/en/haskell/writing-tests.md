---
title:                "Haskell recipe: Writing tests"
programming_language: "Haskell"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## Why
As developers, we often spend countless hours creating complex code to solve problems. But how do we make sure that our code is actually solving those problems correctly? That’s where writing tests comes in. By writing tests, we can ensure that our code is functioning as expected and can easily catch any bugs that may arise as we make changes. This saves time and avoids potential headaches in the future.

## How To
Writing tests in Haskell is quite straightforward. Let’s take a look at an example:

```
haskell
-- This is a function that takes in two numbers and returns their sum.
sum :: Int -> Int -> Int
sum a b = a + b

-- We can write a test for this function using the HUnit library.
sumTest :: Test
sumTest = TestCase $ assertEqual "Sum of 2 and 3 should be 5" (sum 2 3) 5

-- We can then run this test using the `runTestTT` function.
runTestTT sumTest
```

The output of this test should be `Cases: 1 Tried: 1 Errors: 0 Failures: 0`, indicating that our test passed with flying colors. Let’s break down this code:

- Line 1 declares a function named `sum` that takes in two `Int` parameters and returns their sum.
- Line 5 creates a test named `sumTest` using the `TestCase` function from the HUnit library.
- Line 6 uses the `assertEqual` function to compare the expected value (5) with the actual value of calling `sum` with the parameters 2 and 3. If these values do not match, the test will fail.
- Finally, on line 9 we run the test using the `runTestTT` function, which will display the results in the console.

## Deep Dive
As we can see, writing tests in Haskell is quite simple. However, it’s important to note that tests should cover various scenarios and edge cases to ensure that our code is robust. We can also use the `QuickCheck` library to generate random inputs and verify that our code works for a range of values. In addition, we can use `QuickCheck` to test properties of our code by specifying their expected behaviors. By writing thorough tests, we can be confident in the functionality and reliability of our code.

## See Also
- [HUnit documentation](https://hackage.haskell.org/package/HUnit)
- [QuickCheck documentation](https://hackage.haskell.org/package/QuickCheck)
- [How to write good tests in Haskell](https://lexi-lambda.github.io/blog/2018/02/10/an-opinionated-guide-to-haskell-in-2018/#writing-good-unit-tests)