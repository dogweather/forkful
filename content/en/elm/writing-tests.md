---
title:                "Elm recipe: Writing tests"
programming_language: "Elm"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/writing-tests.md"
---

{{< edit_this_page >}}

## Why

As developers, writing tests may seem like an extra step that takes up valuable time. However, writing tests in Elm can actually save you time and effort in the long run. By ensuring that your code is functioning properly through automated tests, you can catch and fix errors before they become bigger issues, ultimately making your code more reliable and maintainable.

## How To

To write tests in Elm, you will need to use the [elm-test](https://github.com/elm-explorations/test) package. This allows you to write unit tests for your functions and also test your user interface (UI) using the [elm-test-browsers](https://github.com/elm-explorations/test/tree/master/elm-test-browsers) package.

First, let's create a simple function to test. In this example, we'll create a function that multiplies two numbers together:

```Elm
multiply : Int -> Int -> Int
multiply x y =
  x * y
```
Next, we can write a test for this function using the `test` function from `elm-test` and the `Test` module:

```Elm
test "multiply" <|
  \() ->
    let
      expected = 12
      actual = multiply 3 4
    in
      Expect.equal expected actual
```

The `test` function takes in a descriptive string as the first argument, and a function that returns a `Test` object as the second argument. In this case, we use the `Expect.equal` function to compare the expected and actual outputs of our `multiply` function.

To run these tests, we can use the `elm-test` command in our terminal:

```
elm-test
```

This will run all the tests in our project and give us a summary of the results.

## Deep Dive

Writing tests in Elm follows a similar structure to writing code. You can use common functions such as `map`, `reduce`, and `filter` to manipulate your test data, just like you would with regular data. It's also important to note that tests are run in isolation, meaning they don't have any access to your application's state. This helps to ensure that your tests are not affected by any external factors.

Additionally, the `Test` module in `elm-test` provides useful functions for testing specific scenarios, such as `Expect.notEqual` for checking non-equality, and `Expect.greaterThan` for checking numerical comparisons.

## See Also

- [elm-test package](https://github.com/elm-explorations/test)
- [elm-test-browsers package](https://github.com/elm-explorations/test/tree/master/elm-test-browsers)
- [Official Elm Guide on testing](https://guide.elm-lang.org/testing/)