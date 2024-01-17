---
title:                "Writing tests"
html_title:           "Elm recipe: Writing tests"
simple_title:         "Writing tests"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?

Writing tests in Elm is the process of creating automated tests that check the functionality of your code. It's an important part of the development process because it helps ensure that your code is working correctly and consistently. By creating tests, you can catch bugs early on and have confidence that your code performs as intended.

## How to:

To write tests in Elm, you will need to use the Elm Test package, which is included in Elm's standard library. Here's a basic example of writing a simple test that checks the result of a function:

```Elm 
import Test exposing (..)

addNum : Int -> Int -> Int
addNum x y =
  x + y
  
tests : Test
tests =
  describe "Addition"
    [ test "1 + 2 equals 3"
        (addNum 1 2
          |> Expect.equal 3
        )
    ]

```

Running these tests will output the following:

``` 

Testing 1 test
PASS ==> Addition
```

The `describe` function creates a named group of tests, while the `test` function sets up a specific test with a name and an `Expect` that specifies the desired outcome. 

## Deep Dive:

The idea of writing tests to check code functionality is not unique to Elm, it is a practice used in many programming languages. However, Elm's approach is slightly different as it uses the concept of "fuzz testing," where it generates random inputs for a function to test various scenarios.

An alternative to writing tests in Elm is using the Elm Debugger, which allows you to check code functionality while running your application. While this can be a helpful tool, it is not a replacement for writing tests as it is limited to checking a single execution of a function.

Under the hood, the Elm Test package uses Elm's built-in fuzz testing features, along with custom `Expect` and `describe` functions, to create a comprehensive testing framework.

## See Also:

To learn more about writing tests in Elm, check out the official documentation for the Elm Test package: https://package.elm-lang.org/packages/elm-explorations/test/latest/

For more details on the process of fuzz testing in Elm, check out this blog post by the creator of Elm, Evan Czaplicki: https://elm-lang.org/news/fuzz-testing

To learn about other useful packages and tools for Elm development, check out the official Elm Guide: https://guide.elm-lang.org/