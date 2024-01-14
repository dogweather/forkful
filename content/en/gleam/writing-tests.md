---
title:                "Gleam recipe: Writing tests"
simple_title:         "Writing tests"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/writing-tests.md"
---

{{< edit_this_page >}}

## Why
Writing tests is an essential part of any software development process. It can help catch bugs and errors in code, ensure that all features work as intended, and provide confidence in the codebase. In short, writing tests can save time and effort in the long run.

## How To
To write tests in Gleam, we first need to create a test module using the `test` keyword. Inside this module, we can define individual test cases using the `#` operator. Let's take a look at an example:

```Gleam
test "addition" {
  assert.equal(2, 1 + 1)
}
```

In this code block, we declared a test case called "addition" and used the `assert.equal` function to check if the result of `1 + 1` is equal to 2. If the assertion fails, an error message will be displayed. Let's see what the output looks like when we run this test:

```
Test "addition" failed
Expected: 2
Actual: 3
```

As we can see, the test failed because the expected and actual values did not match. This indicates that our test is doing its job and catching the error in our code.

We can also use the `assert.not_equal`, `assert.true`, and `assert.false` functions to check for inequality, truthiness, and falsiness, respectively. Additionally, we can use the `#ignore` operator to skip specific test cases, and the `#only` operator to focus on those specific cases during testing.

## Deep Dive
Writing good tests involves following best practices and considering edge cases. Gleam allows us to easily mock external dependencies using the `mock` keyword, which can come in handy when testing code that relies on other services or APIs. Additionally, we can use `test.fixtures` to set up and clean up data for our tests.

When writing tests, it's important to not only cover happy path scenarios but also to test for potential errors and exceptions. This can be done using the `assert_error` function, which checks if a given expression results in an error. Another useful function is `expect_true`, which lets us assert that a function will return `Ok(value)` instead of `Error(error)`.

## See Also
- [Gleam documentation on testing](https://gleam.run/book/tour/testing.html)
- [Writing Automated Tests in Gleam](https://www.youtube.com/watch?v=QcxAJxg76DE)
- [Unit Testing in Gleam with Mocks](https://dev.to/johntyree/unit-testing-in-gleam-470j)