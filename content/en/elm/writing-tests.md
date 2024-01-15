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

## Why

Writing tests in Elm is crucial for ensuring the reliability and functionality of your code. It allows you to catch bugs and errors early on, saving you time and headaches in the long run. Plus, by writing tests, you also document and communicate the intended behavior of your code to other developers.

## How To

Writing tests in Elm is a straightforward process. First, you will need to install the Elm Test package by running the following command in your terminal:

```Elm
elm-install install elm-explorations/test
```

Once the package is installed, you can start writing tests in a separate file from your main code. Here's an example of a test file:

```Elm
module MyTests exposing (..)

import Test exposing (..)
import Expect

add : Int -> Int -> Int
add x y =
    x + y

tests : Test
tests =
    describe "Add Function"
        [ test "adds two numbers correctly" <|
            \_ -> 
                Expect.equal (add 5 7) 12
        ]
```

In this example, we have defined a function named `add` that takes in two numbers and returns their sum. Then, we have written a test case using the `expect` function to check if our `add` function returns the correct result for a given input. 

To run this test, we can use the Elm Test runner by executing the following command in the terminal:

```Elm
elm-test
```

This will output the following result:

```bash
Successfully generated /elm-stuff/generated-code/elm-explorations/test/elmTestOutput.js

TEST RUN PASSED

Duration: 1 millisecond
Passed:   1
Failed:   0
```

If there were any errors or failures in the test, they would be reported in the output. Now, let's dive deeper into the `Expect` module and explore some of its useful functions for writing tests.

## Deep Dive

The `Expect` module contains various functions for making assertions in your tests, such as `equal`, `notEqual`, `beTrue`, `beFalse`, etc. These functions take in the actual and expected values and compare them to determine if the test should pass or fail.

In addition to these basic functions, the `Expect` module also has a `Expectation` type, which allows you to chain multiple expectations and create more complex assertions. Here's an example:

```Elm
Expect.equal 
    (add 10 5)
    |> Expect.andThen (\result -> Expect.lessThan result 20)
```

In this case, we first check if our `add` function returns the correct result and then use the `andThen` function to check if the result is less than 20. If either of these expectations fails, the overall test will fail.

Additionally, the `Test` module provides functions for testing your code's error handling, asynchronous code, and more. You can also define custom expectations using `Expect.satisfy` and `Expect.custom` functions, enabling you to test your code's logic in detail.

Overall, writing tests in Elm is not only essential for ensuring the functionality of your code but also an opportunity to dive deeper into its inner workings and improve its quality.

## See Also

- [Elm Test Package Documentation](https://package.elm-lang.org/packages/elm-explorations/test/latest/)
- [Writing reliable Elm code with tests](https://medium.com/twilio/writing-reliable-elm-code-with-tests-2cd7f58e3ea9)
- [Testing in Elm: Part 1](https://pragmaticstudio.com/blog/2018/11/19/testing-in-elm-part-1)