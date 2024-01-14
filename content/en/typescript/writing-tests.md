---
title:    "TypeScript recipe: Writing tests"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/typescript/writing-tests.md"
---

{{< edit_this_page >}}

## Why

Writing tests may seem like an extra step in the development process, but it is an essential one. Tests help identify bugs and errors in your code, ensuring that your application runs smoothly and functions as intended. By writing tests, you can have greater confidence in your code and catch any issues before they make it to production.

## How To

### Setting Up

To start writing tests in TypeScript, you will need to have Node.js and TypeScript installed on your machine. Once you have those set up, you can create a new directory for your project and run `npm init` to create a `package.json` file.

Next, install the `typescript` and `ts-node` packages using `npm install`. This will allow us to compile and run our TypeScript code without having to manually transpile it.

### Writing a Sample Test

Let's say we have a simple function that adds two numbers together:

```TypeScript
function addNumbers(a: number, b: number): number {
  return a + b;
}
```

To test this function, we can create a new file called `add.test.ts`. In this file, we will import the `assert` module from Node.js and our `addNumbers` function. Then, we can write a basic test that checks if the function returns the correct result for different inputs:

```TypeScript
import assert from 'assert';
import { addNumbers } from './add';

assert.equal(addNumbers(2, 2), 4);
assert.equal(addNumbers(5, 10), 15);
assert.equal(addNumbers(0, 0), 0);
```

We use the `assert.equal` method to compare the expected result with the actual result from our function. If they are not equal, the test will fail and throw an error. You can run this test using the command `npx ts-node add.test.ts`.

### Deep Dive

One of the great things about writing tests is that you can catch bugs and errors early on in the development process. This not only saves time, but it also ensures that your code is more reliable and maintainable.

Additionally, writing tests can also serve as documentation for your code. By creating tests for your functions, you are essentially describing how they should work and what output they should produce. This can be very helpful for other developers who may be working on the same codebase.

Another important aspect of testing is code coverage. Code coverage measures how much of your code is covered by tests. This can help you identify any areas of your code that may need more tests to be written.

## See Also

- [Testing JavaScript with Jest](https://jestjs.io/)
- [An Introduction to TypeScript Testing](https://www.freecodecamp.org/news/an-introduction-to-testing-with-typescript-11a8b4e329ed/)
- [The Importance of Testing in Software Development](https://www.thoughtworks.com/insights/blog/importance-testing-software-development)
- [Setting Up a TypeScript Project](https://dev.to/istoreco/typescript-setup-in-a-nutshell-1pm2)