---
title:    "Gleam recipe: Writing tests"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/gleam/writing-tests.md"
---

{{< edit_this_page >}}

## Why

It's often said that writing tests is just as important as writing code. You may be wondering, why should I put in the extra effort to write tests? Well, there are a few reasons why writing tests can greatly benefit your code and development process.

Firstly, testing helps catch and prevent bugs before they make it into production. By writing tests, you can ensure that your code is functioning as expected and catch any unexpected errors. This can save you a lot of time and effort in the long run, as fixing bugs in production can be much more time-consuming than catching them during development.

Secondly, tests act as documentation for your code. By looking at the tests, other developers can understand the expected behavior of your code without having to dive into the code itself. This can also help you when revisiting your own code after a long period of time.

Overall, writing tests can improve the quality and maintainability of your code, making it a crucial aspect of developing software.

## How To

Now that you understand the importance of writing tests, let's dive into how you can do it in Gleam.

First, we need to create a test module in our project. This can be done by creating a new file with the `.gleam_test` extension. Inside this file, we will define our tests using the `test` function.

Let's look at a simple example:

```Gleam
test "addition" {
  let result = 1 + 1
  assert.equal(result, 2)
}
```

In this code block, we have defined a test called "addition" which will add 1 + 1 and assert that the result is equal to 2. If the result is not equal to 2, the test will fail.

You can also use `assert.ok` to test for truthy values and `assert.error` to test for expected errors. For a complete list of available assertions, check out the Gleam documentation.

To run our tests, we can use the `gleam test` command in the terminal. This will run all the tests in our project and provide us with a summary of the results.

## Deep Dive

Writing tests is all about ensuring that your code is functioning as expected. This means writing tests for both happy and sad paths, as well as testing for edge cases.

In Gleam, you can use pattern matching to help with testing. This allows you to test for different scenarios based on the input provided. You can also use generators to create randomized test data, making your tests more robust.

It's also important to remember to keep your tests up to date as your code changes. This will ensure that the tests are still accurately reflecting the behavior of your code.

## See Also

- [Gleam Documentation](https://gleam.run/)
- [Testing in Gleam](https://gleam.run/articles/testing/)

Happy testing! 🚀