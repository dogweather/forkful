---
title:                "Elm recipe: Writing tests"
simple_title:         "Writing tests"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/writing-tests.md"
---

{{< edit_this_page >}}

## Why

In the world of programming, testing is crucial for ensuring the quality and functionality of our code. However, writing tests for our code can often be seen as a tedious and time-consuming task. So why should we bother adding this extra step in our development process?

The answer is simple: tests help catch bugs and errors early on, allowing us to fix them before they cause larger issues down the road. They also serve as a safety net when making changes to our code, providing assurance that everything is still working as intended. In short, writing tests ultimately saves time and effort in the long run.

## How To

Luckily, Elm makes writing tests a breeze with its built-in `elm-test` package. Let's take a look at an example of how to write a simple test for a function that adds two numbers together:

```
Elm.Test.init
    |> Elm.Test.describe "add function"
        [ Elm.Test.test "adds two positive numbers" <|
            \_ ->
                Elm.Test.expect (add 2 3 == 5)
        ]
    |> Elm.Test.run
```

In this code, we use the `describe` function to give our test a descriptive name, and then use `test` to specify what the expected outcome should be for a given input. Finally, we use `expect` to compare the actual output to the expected result. We can then run our test with the `run` function.

When we run this test, we should see an output similar to the following:

```
╷
│ Expect.add )
╵
Failure: expecting `add 2 3` to equal `5`
Was: `6`
```

This indicates that our test failed because the actual output was not equal to the expected result.

## Deep Dive

Writing tests in Elm also allows us to take advantage of its powerful type system. For example, we can use type annotations on our test functions to ensure that we are passing in the correct types of data. We can also use pattern matching to handle different scenarios and ensure that our code is handling all cases properly.

Additionally, Elm's `elm-test` package offers other helpful functions such as `fuzz` to generate random inputs for our tests and `todo` to temporarily ignore certain tests while we are working on them.

## See Also

To learn more about writing tests in Elm, check out these useful links:

- [Elm Documentation on Testing](https://guide.elm-lang.org/testing/)
- [Elm Syntax Cheatsheet](https://elmprogramming.com/learn-elm-syntax.html)
- [Elm Test Package Documentation](https://package.elm-lang.org/packages/elm-explorations/test/latest/)
- [Elm Testing Best Practices](https://medium.com/@jaybazuzi/elm-testing-best-practices-4c02aa12ac05)