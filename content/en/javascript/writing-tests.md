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

## What & Why?

When writing software, it's important to make sure your code is reliable and free of bugs. Writing tests is a way for programmers to ensure their code is functioning correctly and to identify and fix any issues before they become bigger problems. By writing tests, you can save yourself time and headaches in the long run by catching and addressing errors early on.

## How to:

To write tests in Javascript, we use a framework called Jest. Jest makes it easy to write and run tests for your code. Here's a simple example of a test that checks if the output of a function matches the expected result:

```Javascript
function addNums(a, b) {
  return a + b;
}
test('adds 2 + 3 to equal 5', () => {
  expect(addNums(2, 3)).toBe(5);
});
```

In this example, we define a function called `addNums` that takes in two parameters and returns their sum. Then, we use `test` to define a test case, giving it a description and an arrow function that runs our code and uses `expect` to make an assertion about the result. In this case, we use `toBe` to check if the sum of 2 and 3 is equal to 5.

Jest also provides a helpful feature called "mocking," which allows us to simulate certain values or behaviors for our tests. This can be especially useful when working with external APIs or databases that we don't want to actually call during testing. Here's an example of mocking an API call using `fetch`:

```Javascript
const fetchData = require('./utils');
test('returns the correct data from the API', () => {
  fetchData.mockImplementation(() => Promise.resolve('API data'));
  return fetchData().then(data => expect(data).toBe('API data'));
});
```

In this example, we use `mockImplementation` to define a function that will be used in place of the actual `fetch` call. This allows us to test our code without making any real API calls.

## Deep Dive:

Writing tests has become an essential part of the software development process. In the early days of programming, testing was often an afterthought, with developers mainly focusing on writing the code itself. However, as software became more complex and critical, the need for reliable and bug-free code became increasingly important.

There are alternative testing frameworks available for Javascript, such as Mocha or Jasmine, but Jest has gained popularity due to its simplicity and powerful features. Jest also has built-in support for code coverage, which measures how much of your code is being tested.

When writing tests, it's important to follow best practices, such as keeping tests isolated and independent of each other, and using descriptive test names. It's also essential to regularly run tests and address any failing ones immediately to ensure the integrity of your code.

## See Also:

You can learn more about Jest and how to write tests by reading the official documentation:
https://jestjs.io/

For more information about software testing in general, check out this guide from TestLodge:
https://blog.testlodge.com/what-is-software-testing