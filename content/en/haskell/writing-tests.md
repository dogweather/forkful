---
title:                "Writing tests"
html_title:           "Haskell recipe: Writing tests"
simple_title:         "Writing tests"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?

Writing tests is a common practice among programmers where they create small pieces of code that check if their main code works as expected. These tests help catch any bugs or errors in the code and ensure that it functions correctly. Programmers do this to increase the reliability and quality of their code, ultimately leading to a better user experience.

## How to:

Writing tests in Haskell is a simple process. First, we use the ```HUnit``` library which provides functions for creating and running tests. Then, we write our tests using the ```TestCase``` and ```TestList``` functions. Finally, we use the ```runTestTT```function to execute our tests and see the results.

Example:

```Haskell
import Test.HUnit

-- Test function to check if addition works correctly
testAddition = TestCase (assertEqual "1+1 should be 2" 2 (1+1))

-- Test function to check if list length is correct
testLength = TestCase (assertEqual "Length of the list should be 3" 3 (length [1,2,3]))

-- Combining both tests using TestList
tests = TestList [testAddition, testLength]

-- Running the tests and printing the results
main = runTestTT tests
```

Output:

```Haskell
Cases: 2  Tried: 2  Errors: 0  Failures: 0
Counts {cases = 2, tried = 2, errors = 0, failures = 0}
```

## Deep Dive:

Writing tests has been a common practice in software development for many years. It is based on the concept of test-driven development (TDD), where tests are written before the actual code and serve as a guide to writing functional code. This approach helps catch any potential bugs early on and ensures that the code is thoroughly tested.

Aside from using the ```HUnit``` library, there are other libraries available in Haskell for writing tests such as ```QuickCheck``` and ```Tasty```. These libraries offer different features and options for creating tests, allowing programmers to choose the one that best suits their needs.

When implementing tests, it is essential to follow a naming convention and structure for better organization and readability. This enables easier debugging and maintenance of the tests in the long run.

## See Also:

- Official Haskell documentation on testing: https://www.haskell.org/documentation/#testing
- HUnit library: https://hackage.haskell.org/package/HUnit
- QuickCheck library: https://hackage.haskell.org/package/QuickCheck
- Tasty library: https://hackage.haskell.org/package/tasty