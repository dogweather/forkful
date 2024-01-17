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

## What & Why?
Writing tests in TypeScript involves creating automated tests to check the functionality of your code. This is done to ensure that your code works as intended and catches any potential bugs before deployment. By writing tests, programmers can save time and prevent potential errors in their code.

## How to:
To write tests in TypeScript, you can use various testing libraries such as Jest or Mocha. Below is an example using Jest to test a function that adds two numbers:

```TypeScript
import { add } from './utils';

test('adds 1 + 2 to equal 3', () => {
  expect(add(1, 2)).toBe(3);
});
```

The above code imports a function called `add` from a utils file and then uses the `test` function from Jest to create a test case. The `expect` function is used to verify that the result of `add(1, 2)` equals to `3`.

## Deep Dive:
Writing automated tests has become an essential practice in software development, as it increases code reliability and speed. The concept of unit testing, which involves writing small tests for specific functions, dates back to the 1950s. Over the years, various testing frameworks and libraries have been developed to make testing more accessible for developers.

Besides Jest and Mocha, other popular testing frameworks in TypeScript include Jasmine, Ava, and Tape. These frameworks have different styles and features, so it's essential to research and choose the one that best fits your project.

While writing tests can save time and prevent errors, it is crucial to note that it is not a substitute for debugging and manual testing. Developers should use a combination of different testing methods to ensure the functionality and quality of their code.

## See Also:
- [Jest](https://jestjs.io/)
- [Mocha](https://mochajs.org/)
- [Jasmine](https://jasmine.github.io/)
- [Ava](https://github.com/avajs/ava)
- [Tape](https://github.com/substack/tape)