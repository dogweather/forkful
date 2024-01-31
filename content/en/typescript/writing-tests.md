---
title:                "Writing tests"
date:                  2024-01-19
html_title:           "Arduino recipe: Writing tests"
simple_title:         "Writing tests"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?

Writing tests means crafting code that checks if other code works right. Programmers do it to catch bugs early, save time, and make sure changes don't break stuff.

## How to:

Let's test a simple function using Jest, a popular testing framework for JavaScript and TypeScript.

First, install Jest with TypeScript support:

```bash
npm install --save-dev jest @types/jest ts-jest
```

Add a `jest.config.js`:

```js
module.exports = {
  preset: 'ts-jest',
  testEnvironment: 'node',
};
```

Define a function in `math.ts`:

```typescript
export function add(a: number, b: number): number {
  return a + b;
}
```

Write a test in `math.test.ts`:

```typescript
import { add } from './math';

test('adds 1 + 2 to equal 3', () => {
  expect(add(1, 2)).toBe(3);
});
```

Run tests:

```bash
npx jest
```

Sample output:

```
PASS  ./math.test.ts
✓ adds 1 + 2 to equal 3 (5ms)
```

## Deep Dive

Testing in TypeScript builds off JavaScript testing practices. Here's what makes it special:

- Historical context: TypeScript came to life in 2012. It was meant to add types to JavaScript, making code easier to maintain and test.
- Alternatives: Other than Jest, there's Mocha, Jasmine, and more. Each has unique features; choose based on your needs.
- Implementation details: Tests can live next to code or separately. TypeScript types help with autocompletion and added confidence in tests.

## See Also

- Jest: [Jest Documentation](https://jestjs.io/docs/getting-started)
- Comparison of JS Testing Frameworks: [StateOfJS 2022 Survey](https://2022.stateofjs.com/en-US/libraries/testing/)
