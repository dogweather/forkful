---
date: 2024-02-03 19:03:37.283138-07:00
description: 'How to: Elm uses the `elm-explorations/test` package for writing unit
  and fuzz tests. Start by adding the package to your project.'
lastmod: '2024-03-13T22:45:00.013866-06:00'
model: gpt-4-0125-preview
summary: Elm uses the `elm-explorations/test` package for writing unit and fuzz tests.
title: Writing tests
weight: 36
---

## How to:
Elm uses the `elm-explorations/test` package for writing unit and fuzz tests. Start by adding the package to your project:

```elm
elm install elm-explorations/test
```

Create a test file, say `tests/ExampleTest.elm`, and import the testing modules. Here is a simple test that verifies a function `add : Int -> Int -> Int`:

```elm
module ExampleTest exposing (..)

import Expect
import Test exposing (..)
import YourModuleName exposing (add)

suite : Test
suite =
    describe "A simple addition function"
        [ test "Adding 2 and 3 yields 5" <| 
            \_ -> add 2 3 |> Expect.equal 5
        ]

```

To run your tests, you will need `elm-test`:

```shell
npm install -g elm-test
elm-test
```

This will compile your tests and print the results in your terminal. For the example above, the output should be something like:

```
TEST RUN PASSED

Duration: 42 ms
Passed:   1
Failed:   0
```

For a more complex example, let's say you want to fuzz test the `add` function to ensure it handles a wide range of integer inputs correctly. You would modify your `ExampleTest.elm` as follows:

```elm
module ExampleTest exposing (..)

import Expect
import Fuzz exposing (int)
import Test exposing (..)
import YourModuleName exposing (add)

suite : Test
suite =
    describe "Testing add with fuzzing"
        [ fuzz int "Fuzz testing add with random ints" <| 
            \int1 int2 -> add int1 int2 |> Expect.equal (int1 + int2)
        ]
```

Run `elm-test` again to see the fuzz tests in action. Output will vary with random input but successful tests will indicate no failures:

```
TEST RUN PASSED

Duration: 183 ms
Passed:   100
Failed:   0
``` 

These examples show how to write and run simple unit and fuzz tests in Elm, using the `elm-explorations/test` package. Testing is a vital part of the development process, helping to ensure your Elm applications are reliable and maintain high quality.
