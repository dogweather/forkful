---
title:                "Writing tests"
html_title:           "TypeScript recipe: Writing tests"
simple_title:         "Writing tests"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/writing-tests.md"
---

{{< edit_this_page >}}

## Why

Writing tests is an essential aspect of software development that ensures the quality and reliability of your code. It allows you to catch bugs and errors early on, saving you time and effort in the long run. Writing tests also promotes a more systematic approach to coding, resulting in cleaner and more maintainable code.

## How To

To get started with writing tests in TypeScript, you will need to set up a test framework like Jest or Jasmine. Once you have your framework installed, you can write your tests using the `describe` and `it` functions. Here's a basic example:

```TypeScript
// import necessary functions or classes 
import { add } from './mathFunctions';

// describe block for the "add" function 
describe('add function', () => {
  // it block for testing the addition of two numbers 
  it('adds 2 + 3 to equal 5', () => {
    // call the add function with 2 and 3 as arguments 
    const result = add(2, 3);
    // use the expect function to check if the result is equal to 5 
    expect(result).toEqual(5);
  });
});
```

In the example above, we first imported the `add` function from a separate file called `mathFunctions`. Then, we used the `describe` function to group our tests for the `add` function. Within the `describe` block, we used the `it` function to specify what we expect our `add` function to do. Finally, we used the `expect` function to check if the result of our `add` function is equal to the expected value of 5.

## Deep Dive

When writing tests, it's important to consider different scenarios and edge cases. For example, what happens if we pass in non-numeric values to our `add` function? We can add another `it` block to test this scenario and make sure our function can handle it. Additionally, you can use tools like code coverage to ensure that all of your code is being tested.

Another important aspect of writing tests is using descriptive and precise names for your `describe` and `it` blocks. This makes it easier for others to understand the purpose of your tests and for you to identify any failed tests.

## See Also

- [Jest - Getting Started](https://jestjs.io/docs/en/getting-started)
- [Jasmine - Introduction](https://jasmine.github.io/tutorials/your_first_suite)
- [Code Coverage in TypeScript with Istanbul and Jest](https://medium.com/@jonaskay/code-coverage-in-typescript-with-istanbul-and-jest-e158c3aec6cd)