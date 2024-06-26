---
date: 2024-02-03 19:03:37.303108-07:00
description: "How to: Jest is a popular testing framework that provides a friendly\
  \ API for writing unit tests in JavaScript. It requires minimal configuration and\
  \ comes\u2026"
lastmod: '2024-03-13T22:45:00.436712-06:00'
model: gpt-4-0125-preview
summary: Jest is a popular testing framework that provides a friendly API for writing
  unit tests in JavaScript.
title: Writing tests
weight: 36
---

## How to:


### Native Approach (using Jest)
Jest is a popular testing framework that provides a friendly API for writing unit tests in JavaScript. It requires minimal configuration and comes with features like mock functions, timers, and snapshot testing.

1. **Installation**:

```bash
npm install --save-dev jest
```

2. **Writing a simple test**:

Create a file named `sum.test.js`:

```javascript
const sum = require('./sum'); // Assume this function simply adds two numbers

test('adds 1 + 2 to equal 3', () => {
  expect(sum(1, 2)).toBe(3);
});
```

3. **Running your test**:

```bash
npx jest
```

**Sample Output:**

```plaintext
PASS  ./sum.test.js
✓ adds 1 + 2 to equal 3 (5ms)
```

### Testing Asynchronous Code
Jest makes it easy to test promises and async/await syntax:

```javascript
// asyncSum.js
async function asyncSum(a, b) {
  return Promise.resolve(a + b);
}

// asyncSum.test.js
test('async addition works', async () => {
  await expect(asyncSum(1, 2)).resolves.toBe(3);
});

```

### Using Third-Party Libraries (Mocha & Chai)
Mocha is another popular testing framework, often used with the assertion library Chai for more expressive tests.

1. **Installation**:

```bash
npm install --save-dev mocha chai
```

2. **Writing a test with Mocha and Chai**:

Create `calculate.test.js`:

```javascript
const chai = require('chai');
const expect = chai.expect;

const calculate = require('./calculate'); // A simple calculation module

describe('Calculate', function() {
  it('should sum two values', function() {
    expect(calculate.sum(5, 2)).to.equal(7);
  });
});
```

3. **Running your tests with Mocha**:

Add a script in your `package.json`:

```json
"scripts": {
  "test": "mocha"
}
```

Then execute:

```bash
npm test
```

**Sample Output:**

```plaintext
  Calculate
    ✓ should sum two values


  1 passing (8ms)
```

These examples illustrate basic test writing and execution in JavaScript. Adopting a testing framework like Jest or Mocha with Chai can provide a solid foundation for robust application testing, helping to ensure your code functions as intended across updates and refactorings.
