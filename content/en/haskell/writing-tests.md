---
title:                "Writing tests"
html_title:           "Arduino recipe: Writing tests"
simple_title:         "Writing tests"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?

Writing tests checks if code does what it's supposed to. Testing catches bugs, helps maintain code, and makes sure changes don’t break stuff.

## How to:

Haskell uses HUnit for basic unit tests, and QuickCheck for property-based tests. Here's a quick HUnit example:

```haskell
import Test.HUnit

testList :: Test
testList = TestList [TestCase (assertEqual "Should add numbers" 4 (2 + 2)),
                     TestCase (assertEqual "Should subtract numbers" 0 (2 - 2))]

main :: IO Counts
main = runTestTT testList
```

Run it, and it shows:

```
Cases: 2  Tried: 2  Errors: 0  Failures: 0
```

QuickCheck example:

```haskell
import Test.QuickCheck

prop_RevRev :: [Int] -> Bool
prop_RevRev xs = reverse (reverse xs) == xs

main :: IO ()
main = quickCheck prop_RevRev
```

Sample output might read:

```
+++ OK, passed 100 tests.
```

## Deep Dive

Testing started with early programming, but got serious with the rise of TDD in the '00s. Haskell's pure functions make it great for testing. Alternatives to HUnit/QuickCheck include doctest and Hedgehog. HUnit is like JUnit from Java. QuickCheck automates test case generation, checking for properties you define.

## See Also

- HUnit documentation: [http://hackage.haskell.org/package/HUnit](http://hackage.haskell.org/package/HUnit)
- QuickCheck on Hackage: [http://hackage.haskell.org/package/QuickCheck](http://hackage.haskell.org/package/QuickCheck)
- Intro to Haskell Testing: [https://hspec.github.io/](https://hspec.github.io/)
- "Real World Haskell" by Bryan O'Sullivan, Don Stewart, and John Goerzen: [http://book.realworldhaskell.org/](http://book.realworldhaskell.org/)
