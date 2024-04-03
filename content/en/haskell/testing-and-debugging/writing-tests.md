---
date: 2024-02-03 19:03:27.757892-07:00
description: "How to: Haskell supports various testing frameworks, but two popular\
  \ ones are `Hspec` and `QuickCheck`. Hspec allows you to define human-readable\u2026"
lastmod: '2024-03-13T22:45:00.131731-06:00'
model: gpt-4-0125-preview
summary: Haskell supports various testing frameworks, but two popular ones are `Hspec`
  and `QuickCheck`.
title: Writing tests
weight: 36
---

## How to:
Haskell supports various testing frameworks, but two popular ones are `Hspec` and `QuickCheck`. Hspec allows you to define human-readable specifications for your code, while QuickCheck lets you generate tests automatically by describing properties that your code should satisfy.

### Using Hspec
First, add `hspec` to your build tool configuration (e.g., `stack.yaml` or `cabal` file). Then, import `Test.Hspec` and write tests as specifications:

```haskell
-- file: spec/MyLibSpec.hs
import Test.Hspec
import MyLib (add)

main :: IO ()
main = hspec $ describe "MyLib.add" $ do
  it "adds two numbers" $
    add 1 2 `shouldBe` 3

  it "returns the first number when adding zero" $
    add 5 0 `shouldBe` 5
```

Then, run your tests using your build tool, resulting in an output that might look like:

```
MyLib.add
  - adds two numbers
  - returns the first number when adding zero

Finished in 0.0001 seconds
2 examples, 0 failures
```

### Using QuickCheck
With QuickCheck, you express properties that your functions should satisfy. Add `QuickCheck` to your project configuration, then import it:

```haskell
-- file: test/MyLibProperties.hs
import Test.QuickCheck
import MyLib (add)

prop_addAssociative :: Int -> Int -> Int -> Bool
prop_addAssociative x y z = x + (y + z) == (x + y) + z

prop_addCommutative :: Int -> Int -> Bool
prop_addCommutative x y = x + y == y + x

main :: IO ()
main = do
  quickCheck prop_addAssociative
  quickCheck prop_addCommutative
```

Running these tests will auto-generate inputs to check the specified properties:

```
+++ OK, passed 100 tests.
+++ OK, passed 100 tests.
```

In both Hspec and QuickCheck examples, the test suites serve as executable documentation that can automatically verify the correctness of your code.
