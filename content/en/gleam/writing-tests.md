---
title:    "Gleam recipe: Writing tests"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Why
Writing tests is an essential part of programming in any language, including Gleam. It allows developers to ensure their code is functioning as expected and catch any errors before they are introduced into production.

## How To

Writing tests in Gleam is straightforward and follows a similar structure to other languages. Here is an example of a simple test case for a function that calculates the average of two numbers:

```Gleam
fn average(x: Float, y: Float) -> Float {
    (x + y) / 2.0
}

// Test case
fn average_test() {
    let result = average(2.0, 4.0)

    assert result == 3.0
}
```

In this example, we have defined a function called `average` that takes two Float values and calculates their average. Then, we have created a test case called `average_test`, which calls the `average` function with two known values and asserts that the result is equal to the expected value. 

To run this test case, we can use the `gleam test` command in our terminal. If all goes well, we should see a message indicating that the test has passed.

```
Running 1 test

OK 1 test complete. (0.002s)
```

In the event that the test fails, the output will indicate which assertion failed and what the actual result was.

## Deep Dive

Gleam also provides useful functions and macros for more complex testing scenarios. For example, the `assert_true` macro is useful for testing Boolean expressions, and the `assert_throw` function is helpful for testing code that is expected to throw an error. Additionally, the `suite` macro allows for the creation of multiple test cases, making it easier to organize and run tests.

It is essential to thoroughly test all of your code, including edge cases and potential errors. Gleam's testing capabilities make it easier to identify and fix any problems, leading to more robust and reliable code.

## See Also

For more information on writing tests in Gleam, check out the official documentation and the community forum: 
- [Writing tests in Gleam](https://gleam.run/book/getting-started/tests.html)
- [Gleam community forum](https://community.gleam.run/)