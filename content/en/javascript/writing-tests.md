---
title:                "Writing tests"
date:                  2024-01-19
html_title:           "Arduino recipe: Writing tests"
simple_title:         "Writing tests"

category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
Writing tests in programming checks if code behaves as expected—like a quiz for your functions. Programmers write tests to catch bugs early, saving headaches and money.

## How to:
Imagine a simple function to add two numbers in JavaScript:

```javascript
function add(a, b) {
  return a + b;
}
```

To test this, you might use a testing framework like Jest. Here's how you'd write a basic test:

```javascript
const add = require('./add'); // assuming add function is in 'add.js'

test('adds 1 + 2 to equal 3', () => {
  expect(add(1, 2)).toBe(3);
});
```

Run the tests, and Jest will tell you if the `add` function passed:

```plaintext
PASS  ./add.test.js
✓ adds 1 + 2 to equal 3 (5ms)
```

## Deep Dive
Historically, testing was manual, tedious, and error-prone. The rise of automated testing in the late 20th century improved this, with TDD (Test-Driven Development) becoming a key methodology. Alternatives to Jest include Mocha, Jasmine, and QUnit, among others. The main implementation detail in writing tests is the assertion: a statement that checks if something is true. If assertions pass, your test passes.

## See Also
- Jest: https://jestjs.io/
- Test-Driven Development: https://en.wikipedia.org/wiki/Test-driven_development
- Mocha: https://mochajs.org/
- Jasmine: https://jasmine.github.io/
- QUnit: https://qunitjs.com/
