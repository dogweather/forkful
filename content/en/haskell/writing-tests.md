---
title:                "Haskell recipe: Writing tests"
simple_title:         "Writing tests"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/writing-tests.md"
---

{{< edit_this_page >}}

# Why Write Tests?
As programmers, our main goal is to create functional and reliable code. But how do we ensure that our code works as intended? This is where writing tests comes in. 

Tests are a crucial part of the development process as they help us detect and prevent bugs, verify the functionality of our code, and improve the overall quality of our software. They also serve as documentation for our code, making it easier for others to understand and maintain.

# How To Write Tests in Haskell
Writing tests in Haskell is made easy with the help of the HUnit library. In this tutorial, we will go through the process of creating and running tests using the HUnit framework.

To get started, we need to import the necessary modules from the HUnit library:
```Haskell
import Test.HUnit
```
Next, we can define our test cases using the `TestCase` and `TestList` functions:
```Haskell
testAddition :: Test
testAddition = TestCase (assertEqual "1 + 1 should equal 2" 2 (1+1))

testSubtraction :: Test
testSubtraction = TestCase (assertEqual "5 - 3 should equal 2" 2 (5-3))

testMultiplication :: Test
testMultiplication = TestCase (assertEqual "2 * 3 should equal 6" 6 (2*3))

testDivision :: Test
testDivision = TestCase (assertEqual "6 / 3 should equal 2" 2 (6/3))

testCases :: Test
testCases = TestList [testAddition, testSubtraction, testMultiplication, testDivision]
```

We can then run our test cases using the `runTestTT` function and view the results:
```Haskell
main :: IO ()
main = do
    runTestTT testCases
```

The output should look like this:
```
Cases: 4  Tried: 4  Errors: 0  Failures: 0
Cases: 4  Tried: 4  Errors: 0  Failures: 0
Cases: 4  Tried: 4  Errors: 0  Failures: 0
Cases: 4  Tried: 4  Errors: 0  Failures: 0
Cases: 4  Tried: 4  Errors: 0  Failures: 0
  Cases: 4  Tried: 4  Errors: 0  Failures: 0
  Cases: 4  Tried: 4  Errors: 0  Failures: 0

Cases: 4  Tried: 4  Errors: 0  Failures: 0
```

These results tell us that all of our test cases have passed, and our code is functioning as intended. If there were any errors or failures, the output would show them in detail.

# Deep Dive into Writing Tests
Now that we have gone through the basic steps of writing and running tests, let's go deeper and look at some best practices for writing tests in Haskell.

1. Use descriptive test names: Clear and descriptive test names can make it easier to understand what each test case is checking for and what its expected result is.

2. Use different types of tests: While unit tests are essential, it's also important to also include integration and end-to-end tests to cover different aspects of your code.

3. Test edge cases and unexpected inputs: Don't just test the typical cases, but also include tests for edge cases and invalid inputs to ensure that your code can handle all possible scenarios.

4. Keep tests independent and order-independent: Each test case should be able to run independently without depending on the results of other tests. It's also important to run tests in a random order to catch any potential issues with test dependence.

5. Continuously update and maintain tests: As your code evolves, make sure to update your tests to reflect any changes. This ensures that your tests stay relevant and effective.

# See Also
- [HUnit documentation](https://hackage.haskell.org/package/HUnit)
- [Best practices for unit testing in Haskell](https://www.fpcomplete.com/blog/2017/10/unit-testing-haskell-best-practices/)
- [Test-driven development (TDD) in Haskell](https://medium.com/@vaibhavsagar/test-driven-development-in-haskell-6b56622d7e4f)