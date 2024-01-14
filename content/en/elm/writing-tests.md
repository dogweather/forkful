---
title:    "Elm recipe: Writing tests"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Why

When it comes to software development, testing is crucial for ensuring the quality and functionality of your code. Writing tests not only helps catch bugs and errors, but it also serves as a form of documentation for your code. In the long run, writing tests can save you time and headaches by avoiding potential issues before they arise. 

## How To

To start writing tests in Elm, you will need to import the `Test` module.

```
import Test exposing (Test, test)
```
Next, you can use the `test` function to create a test case. For example, if you want to test a function that adds two numbers, you can write the following test case:

```
addTest : Test
addTest =
   test "adding two numbers" <|
      \() ->
         expect (add 2 3) toEqual 5

```

In this code, the `test` function takes two arguments - a description of what is being tested and a function that defines the test. The `expect` function is used to define the expected output of the test.

To actually run the tests, you can use the `Test` module's `run` function:

```
tests : Test
tests =
  describe "Calculator tests"
    [ addTest
    , subtractTest
    ]

main : Program Never
main =
  Test.run tests
```

This will produce an output similar to the following:

```
Calculator tests
  - adding two numbers: OK
  - subtracting two numbers: FAILED

```

## Deep Dive

When writing tests, it is important to cover as many edge cases as possible to ensure the reliability of your code. Elm's built-in `expect` function can handle different types of expectations such as equality, inequality, and exceptions. You can also group related tests together using the `describe` function.

Additionally, Elm also provides the `fuzz` function to generate random test cases for greater coverage. This is particularly useful for testing functions that take in unexpected inputs.

Another helpful feature is Elm's ability to use modules within tests. This allows you to write tests for private or helper functions within your code.

## See Also

For more information and examples on writing tests in Elm, check out these resources:

- Official Elm Testing Guide: https://guide.elm-lang.org/testing/
- Elm Test package documentation: https://package.elm-lang.org/packages/elm-explorations/test/latest/
- Testing Elm Applications with elm-test: https://thoughtbot.com/blog/testing-elm-applications-with-elm-test