---
title:    "Haskell recipe: Writing tests"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## Why
Writing tests is an essential aspect of programming, especially in functional languages like Haskell. It allows us to catch and fix errors early on, ensuring that the code works as intended. Testing also helps with maintaining and refactoring code, reducing the chances of introducing bugs.

## How To
Writing tests in Haskell is a simple process that involves defining test cases and running them. Let's consider a simple example of a function that checks if the given number is even or odd.

```Haskell
isEven :: Int -> Bool
isEven n = n `mod` 2 == 0
```

To test this function, we can define a test case using the `assert` function from the `Test.HUnit` library.

```Haskell
testIsEven :: Test
testIsEven = TestCase $ assertEqual "Should return true for even number" True (isEven 4)
```

Next, we need to run the test using the `runTestTT` function from the `Test.HUnit.Text` module.

```Haskell
main :: IO Counts
main = runTestTT $ TestList [testIsEven]
```

Running the program will show the test results like this:

```
Cases: 1 Tried: 1 Errors: 0 Failures: 0
Counts {cases = 1, tried = 1, errors = 0, failures = 0}
```

We can add more test cases to cover different scenarios and make sure our function is working correctly.

## Deep Dive
There are various testing frameworks available in Haskell, such as HUnit, QuickCheck, and Tasty. Each has its own set of features and capabilities, allowing for different testing approaches. Some frameworks also support automated property-based testing, which generates test cases based on defined properties of the code.

It's essential to write tests that cover different scenarios, including edge cases, boundary values, and invalid inputs. This ensures that the code is robust and can handle various inputs without breaking. It's also crucial to regularly run tests and update them whenever there are changes to the code.

## See Also
- [HUnit documentation](https://hackage.haskell.org/package/HUnit)
- [QuickCheck documentation](https://hackage.haskell.org/package/QuickCheck)
- [Tasty documentation](https://hackage.haskell.org/package/tasty)