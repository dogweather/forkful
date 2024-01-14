---
title:                "Gleam recipe: Writing tests"
programming_language: "Gleam"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/writing-tests.md"
---

{{< edit_this_page >}}

## Why

In the world of software development, writing tests is often seen as a monotonous and time-consuming task. However, in reality, writing tests plays a crucial role in ensuring the quality and functionality of your code. By writing tests, you can catch bugs early on, ensure that code changes don't break existing functionalities, and build more reliable software. So, if you want to level up your coding game, it's time to start writing tests!

## How To

Writing tests in Gleam is straightforward and follows a simple syntax. Let's take a look at an example of a simple function and how we can write tests for it.

```Gleam
fn add(x, y) {
  x + y
}
```

To write tests for this function, we first need to import the `gleam/test` module. This module provides us with functions and macros for writing and running tests.

```Gleam
import gleam/test
```

Next, we can use the `test` macro to define our test cases. The `test` macro takes a name for the test and a function that contains the assertions for that test.

```Gleam
test "add tests" {
  fn() {
    gleam/test/assert.equal(add(2, 3), 5)
    gleam/test/assert.equal(add(5, 7), 12)
  }
}
```

In the above code, we have defined two test cases that ensure our `add` function is working correctly. The `gleam/test/assert.equal` function takes two arguments and checks if they are equal. If the assertion fails, an error will be thrown, and the test will fail.

After defining our tests, we can use the `gleam/test/runner` function to run our tests.

```Gleam
fn main() {
  gleam/test/runner.test([
    "add tests",
  ])
}
```

When we run our code, we will get an output that looks like this:

```
[FAIL] add tests
  expected 5 to equal 6 at line 7

[OK] [1 / 2] add tests
  expected 12 to equal 12

Ran 2 tests in 0.0 seconds
Tests passed: 1
Tests failed: 1
```

By looking at the output, we can see that one of our tests failed because we made a mistake in our `add` function. Thanks to our tests, we were able to catch this bug and fix it before it caused any issues in our code.

## Deep Dive

There are a few different ways you can write tests in Gleam, depending on the types of tests you want to write. For more complex tests, you can use the `gleam/test/assert.match` function, which allows you to use regular expressions to match values. Additionally, you can also use the `gleam/test/assert.ok` function to check if a value is truthy or falsy.

It's also important to note that you can organize your tests into different modules to keep your codebase clean and maintainable. You can even define your own custom assertions if the built-in ones don't meet your needs.

But perhaps the most essential aspect of writing tests is to write tests for as much code as possible. The more tests you have, the more confident you can be in your code's functionality and the less time you will spend debugging and fixing bugs.

## See Also

- [Gleam Testing Guide](https://gleam.run/book/testing.html)
- [Gleam Documentation](https://gleam.run/documentation.html)
- [Gleam Community](https://gleam.run/community.html)