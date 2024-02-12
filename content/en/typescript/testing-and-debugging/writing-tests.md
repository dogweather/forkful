---
title:                "Writing tests"
aliases:
- /en/typescript/writing-tests/
date:                  2024-02-03T19:03:31.655800-07:00
model:                 gpt-4-0125-preview
simple_title:         "Writing tests"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
Writing tests in TypeScript involves creating automated scripts to verify the functionality and correctness of your code. Programmers do it to ensure reliability, quickly catch bugs, and facilitate maintainable code growth, as TypeScript's static typing adds a level of predictability to JavaScript testing.

## How to:
TypeScript works harmoniously with most JavaScript testing frameworks. For demonstration purposes, we'll use Jest, a popular testing framework, due to its zero-configuration setup for TypeScript projects.

First, ensure you have Jest and the necessary TypeScript types installed:

```bash
npm install --save-dev jest typescript ts-jest @types/jest
```

Next, set up Jest to work with TypeScript by modifying the `jest.config.js` or if creating a new one:

```javascript
module.exports = {
  preset: 'ts-jest',
  testEnvironment: 'node',
};
```

Now, let's write a simple function and a test for it. Consider a `sum.ts` file with the following function:

```typescript
// sum.ts
export function sum(a: number, b: number): number {
  return a + b;
}
```

Create a test file named `sum.test.ts`:

```typescript
// sum.test.ts
import { sum } from './sum';

test('adds 1 + 2 to equal 3', () => {
  expect(sum(1, 2)).toBe(3);
});
```

Run your tests with:

```bash
npx jest
```

Sample output indicating a passed test should look something like:

```plaintext
 PASS  ./sum.test.ts
  ✓ adds 1 + 2 to equal 3 (2 ms)
```

For asynchronous code, Jest accommodates with `async/await`. Suppose you have an asynchronous `fetchData` function:

```typescript
// asyncFunctions.ts
export async function fetchData(): Promise<string> {
  return "data";
}
```

Your test using async functions:

```typescript
// asyncFunctions.test.ts
import { fetchData } from './asyncFunctions';

test('fetches data successfully', async () => {
  expect(await fetchData()).toBe('data');
});
```

When running your tests, Jest will wait for the promise to resolve, correctly testing asynchronous operations.

Remember, effective testing includes writing multiple tests for different scenarios, including edge cases, to ensure your TypeScript code behaves as expected.
