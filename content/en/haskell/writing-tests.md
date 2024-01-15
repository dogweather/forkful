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

## Why

Writing tests is an important part of software development, regardless of the programming language being used. In Haskell, tests serve as a way to ensure the correctness of the code and catch any potential bugs early on in the development process. It also serves as a form of documentation for future developers to understand the code base.

## How To

To write tests in Haskell, we will be using the `HSpec` library. Let's begin by creating a new Haskell project using `stack`.

```Haskell
stack new test-project simple
cd test-project
```

Next, we will add `HSpec` as a dependency in our `stack.yaml` file. 

```Haskell
# Additional packages for testing
- hspec
```

We can now define our tests in a separate file called `Spec.hs` inside the `test` directory.

```Haskell
import Test.Hspec

main :: IO ()
main = hspec $ do

  describe "add" $ do
    it "adds two numbers correctly" $ do
      add 2 3 `shouldBe` 5

add :: Int -> Int -> Int
add x y = x + y
```

Here, we have defined a test suite using the `describe` and `it` functions. Within the `it` function, we are using the `shouldBe` assertion to check if the output of our `add` function is correct. We can run our tests by using the `stack test` command.

```
test-project: Test suite test-project-test passed
```

We can also use `HSpec` to run only specific tests or test cases by using the `runSpec` function. 

```Haskell
main :: IO ()
main = hspec $ do

  describe "add" $ do
    it "adds two numbers correctly" $ do
      add 2 3 `shouldBe` 5

  describe "subtract" $ do
    it "subtracts two numbers correctly" $ do
      subtract 5 2 `shouldBe` 3

      runSpec subtract
```

This will only run the `subtract` test and ignore the `add` test. 

## Deep Dive

Writing tests in Haskell allows us to use the type system to our advantage. For example, we can define custom types for our inputs and outputs to ensure that our functions are only accepting the correct types. In addition, tests in Haskell are also composable, meaning we can reuse tests for different parts of our code base.

We can also use `HSpec`'s built-in functions such as `shouldBe` and `shouldNotBe` to check for equality or inequality. There are also other assertions available, such as `shouldReturn` and `shouldSatisfy`, for different scenarios.

## See Also

- Official HSpec Documentation: https://hspec.github.io/
- Introduction to QuickCheck: https://www.ircmaxell.com/blog/2012/07/introduction-to-quickcheck.html
- Writing Testable Code: https://wiki.haskell.org/Writing_testable_code