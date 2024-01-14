---
title:                "TypeScript recipe: Writing tests"
programming_language: "TypeScript"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/writing-tests.md"
---

{{< edit_this_page >}}

## Why
As developers, writing tests is an essential part of our job. Not only does it ensure that our code is functioning properly, but it also helps with future maintenance and debugging. In this blog post, we will dive into the world of testing in TypeScript and learn why it is important.

## How To
To start writing tests in TypeScript, we need to install a testing framework. One popular option is Jest, which provides a simple and intuitive API for testing. Let's create a simple function that adds two numbers and write a test for it.

```TypeScript
// Our add function
function add(a: number, b: number) {
  return a + b;
}

// Test case for add function
test('adds two numbers correctly', () => {
  expect(add(2, 3)).toBe(5);
});
```

In the above code, we have defined our add function and then written a test case using the `test` function provided by Jest. The `expect` statement compares the result of our `add` function with the expected result using the `toBe` matcher. If the values aren't equal, the test will fail and provide a detailed error message.

Besides the `toBe` matcher, Jest also provides other useful matchers such as `toEqual` for deep equality checks and `toHaveLength` for checking the length of an array or string.

Now that we have seen a simple example, let's take a deeper look into writing tests.

## Deep Dive
One of the key principles of testing is having a good test structure. We usually want to have one test file per code file, with each test case focusing on a specific function or functionality. This helps with debugging and maintaining our tests in the future.

In addition to unit tests, we can also write integration tests to check the behavior of multiple components working together. Jest provides us with different ways to group and organize our tests, allowing us to run specific groups of tests or only a single test.

Another important aspect is writing tests that are independent of each other. Each test should be able to run on its own without depending on the result of another test. This ensures that our tests are reliable and can be run in any order.

Lastly, we should aim for a high code coverage when writing tests. Code coverage is a metric that measures the percentage of code that is covered by our tests. This helps us identify any areas of our code that might not be tested and may require additional tests.

## See Also
- [Jest Documentation](https://jestjs.io/docs/en/getting-started)
- [Introduction to Testing in JavaScript with Jest](https://www.digitalocean.com/community/tutorials/how-to-write-and-run-tests-with-jest)
- [TDD vs BDD vs ATDD: What’s the Difference?](https://www.qasymphony.com/blog/tdd-vs-bdd-vs-atdd/)