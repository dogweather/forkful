---
title:                "Writing tests"
html_title:           "Gleam recipe: Writing tests"
simple_title:         "Writing tests"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
Testing is a crucial aspect of programming where we write small snippets of code to check if our main code works correctly. Programmers do it to catch bugs early, prevent regressions, and improve code quality.

## How to:
To write tests, we use the built-in ```Gleam``` testing module. It has a ```test/1``` function that takes a ```Gleam.Test``` argument, under which we use the ```assert_eq``` function to verify that the output matches the expected value. Let's look at a simple example:

```
test "addition adds two numbers" {
  assert_eq 2 (1 + 1)
}
```

## Deep Dive:
In the past, testing was typically done manually, which was both time-consuming and prone to errors. However, with the rise of automated testing frameworks like ```Gleam```, writing tests has become easier and more efficient. Alternatives to writing tests include using third-party libraries or writing custom functions to check for expected values. Additionally, writing tests also encourages modular code design and helps with code refactoring.

## See Also:
- [Gleam testing documentation](https://gleam.run/documentation/tests)
- [Article on the importance of testing in software development](https://www.techopedia.com/2/33901/software/qa/why-is-software-testing-important)