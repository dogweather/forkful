---
title:                "Writing tests"
html_title:           "Arduino recipe: Writing tests"
simple_title:         "Writing tests"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
Writing tests means crafting code that checks if your main code works as expected. Programmers test to catch bugs early, ensure functionality, and make future changes less risky.

## How to:
Elm uses `elm-test` for writing tests. Here's a small test for a function `add` that sums two numbers:

```Elm
import Expect
import Test exposing (..)
import AddingModule exposing (add)

suite : Test
suite =
    describe "AddingModule"
        [ test "add function test" <|
            \_ -> Expect.equal (add 1 2) 3
        ]

-- To run tests, use the following command:
-- elm-test
```

If `add` works correctly, the output will be:

```
TEST RUN PASSED

Duration: 42 ms
Passed:   1
Failed:   0
```

## Deep Dive
Elm's testing framework, `elm-test`, provides a fast, reliable way to write unit tests. It encourages TDD (Test-Driven Development). Before `elm-test`, alternatives like `elm-check` existed, but weren't as integrated. Implementation-wise, `elm-test` uses pure functions free of side effects, aligning perfectly with Elm's architecture.

## See Also
- Elm's official testing documentation: https://package.elm-lang.org/packages/elm-explorations/test/latest/
- An article on Elm's testing patterns: https://elmprogramming.com/testing.html
- The `elm-test` package on GitHub: https://github.com/elm-explorations/test
