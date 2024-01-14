---
title:    "Elm recipe: Writing tests"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/elm/writing-tests.md"
---

{{< edit_this_page >}}

## Why

Writing tests for your Elm code may seem like an unnecessary hassle at first, but it can actually save you time and effort in the long run. Not only do tests help catch bugs and errors early on, they also act as a safety net when making changes to your codebase.

## How To

To start writing tests in Elm, you'll first need to set up your project with the `elm-test` package. Then, you can use the `test` and `expect` functions to define your tests. Let's look at an example:

```Elm
test "addition test" : 
    expect (1 + 1) toBe 2
```

In this test, we use the `expect` function to assert that the result of `1 + 1` is equal to 2. We can also use `toBeTrue` and `toBeFalse` to check for boolean values, and `toEqual` for more complex data types like lists or records.

## Deep Dive

There are a few key things to keep in mind when writing tests in Elm. One is to make sure your tests are independent and idempotent - meaning they can be run in any order and multiple times without changing the result. Another is to think about what your tests are actually testing, and not just repeating the logic of your code.

You can also use the `testProperty` function to create randomized property-based tests, which can help catch edge cases in your code. And don't forget to include tests for any error cases or edge cases that may arise.

## See Also

Here are some links for further reading on writing tests in Elm:

- [Elm documentation on testing](https://guide.elm-lang.org/testing/)
- [Property-based testing in Elm](https://dev.to/andrewMacmurray/property-based-testing-in-elm-52kj)
- [Idiomatic Elm testing](https://dev.to/jessicabrys/idiomatic-elm-testing-the-elm-test-path-198o)

Happy testing!