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

## Why

Writing tests is an essential part of software development, as it helps ensure code stability and catch bugs early on. By writing tests, you can have confidence in the functionality of your code and prevent unexpected issues from arising in the future.

## How To

To start writing tests in Gleam, you will need to use the `gleam-test` library, which can be installed through the Gleam package manager. Once installed, you can import it into your project with the following code:

```Gleam
import gleam/test
```

Next, you can write your first test using the `test` function, which takes in a description and a function that contains your test code. Here's an example of a simple test that checks if the result of adding two numbers is correct:

```Gleam
test "Adding two numbers should return the correct result"() {
  let result = 2 + 2
  expect(result).toBe(4)
}
```

You can also use other assertions, such as `toBeNil`, `toEqual`, and `toContain`, to check for more specific conditions in your tests.

Once you have written your tests, you can run them by executing the command `gleam test` in your project directory. This will run all the tests and provide a report on the number of passing and failing tests.

## Deep Dive

Writing tests in Gleam follows the Arrange-Act-Assert (AAA) pattern, where you set up the initial state of your code, perform an action, and then assert that the result is as expected. It is essential to follow this pattern to ensure that your tests are organized and easy to understand.

Additionally, Gleam has other testing libraries such as `gleam-expect` that provide more advanced assertions, and `gleam-check` that can generate random inputs for testing.

It is also recommended to write multiple tests for each function and to update them when making changes to your code, as it helps catch bugs and ensure code stability.

## See Also

- [Official Gleam Documentation](https://gleam.run/documentation/)
- [Writing Tests in Gleam Tutorial](https://github.com/codenoid/Gleam-talk/blob/master/src/guide/writing-tests.md)
- [Gleam Testing Example Project](https://github.com/mjm/greeter)