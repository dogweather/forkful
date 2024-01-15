---
title:                "Writing tests"
html_title:           "Javascript recipe: Writing tests"
simple_title:         "Writing tests"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/writing-tests.md"
---

{{< edit_this_page >}}

## Why

Writing tests for code may seem like an extra and tedious step, but it can actually save you time and effort in the long run. By writing tests, you can catch errors and bugs early on, making it easier to fix them before they cause bigger issues. Additionally, tests can help ensure that new changes or updates to your code do not break existing features.

## How To

To write tests in Javascript, you can use a testing library like Jest or Mocha. These libraries provide a simple and efficient way to create and run tests for your code. Let's take a look at an example of how to write a basic test using Jest.

```Javascript
// Example function to be tested
function addition(a, b) {
  return a + b;
}

// Jest test
test('adds 1 + 2 to equal 3', () => {
  expect(addition(1, 2)).toBe(3);
});
```
In this example, we have a simple function that adds two numbers together. We then use the `test()` function from Jest to create a test case. Inside the function, we provide a description of what the test is checking for and then use the `expect()` function to define the expected output of our function. In this case, we expect the result of adding 1 and 2 to be 3. If the output matches our expectation, the test will pass.

You can also use `describe()` to group related tests together and make your test suite more organized. Here's a modified example of the previous test using `describe()`:

```Javascript
// Example function to be tested
function addition(a, b) {
  return a + b;
}

// Jest test
describe('Addition', () => {
  test('adds 1 + 2 to equal 3', () => {
    expect(addition(1, 2)).toBe(3);
  });
});
```
This time, we have added a `describe()` function to group our tests related to addition together. This makes it easier to identify which tests belong to which function.

## Deep Dive

When writing tests, it's important to consider edge cases and different inputs to ensure your code is robust. This means testing for unexpected input, extreme values, and any potential errors that may occur. You can also use mocking libraries like Sinon.js to simulate certain scenarios and test different code paths.

It's also important to regularly review and update your tests as your code evolves. As new features are added or existing ones are modified, be sure to update your tests accordingly to ensure they accurately reflect the expected behavior of your code.

## See Also

- [Jest Documentation](https://jestjs.io/)
- [Mocha Documentation](https://mochajs.org/)
- [Sinon.js Documentation](https://sinonjs.org/)