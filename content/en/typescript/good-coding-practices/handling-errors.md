---
date: 2024-01-21 21:19:19.875424-07:00
description: "Handling errors is about expecting the unexpected; it's how we manage\
  \ when things go wrong in our code. We do it to avoid crashes and to give users\
  \ a\u2026"
lastmod: '2024-02-25T18:49:56.289072-07:00'
model: gpt-4-1106-preview
summary: "Handling errors is about expecting the unexpected; it's how we manage when\
  \ things go wrong in our code. We do it to avoid crashes and to give users a\u2026"
title: Handling errors
---

{{< edit_this_page >}}

## What & Why?
Handling errors is about expecting the unexpected; it's how we manage when things go wrong in our code. We do it to avoid crashes and to give users a smooth experience, even when the unexpected happens.

## How to:
In TypeScript, handling errors often involves `try`, `catch`, and `finally` blocks.

```typescript
function riskyOperation() {
  throw new Error("Something went wrong!");
}

function handleErrors() {
  try {
    riskyOperation();
  } catch (error) {
    console.error("Caught an error:", error.message);
  } finally {
    console.log("This always runs, error or not.");
  }
}

handleErrors();
```

Sample output:

```
Caught an error: Something went wrong!
This always runs, error or not.
```

Async example with promises:

```typescript
async function asyncRiskyOperation() {
  return new Promise((resolve, reject) => {
    // Simulate an error
    reject("Failed miserably");
  });
}

async function handleAsyncErrors() {
  try {
    await asyncRiskyOperation();
  } catch (error) {
    console.error("Caught async error:", error);
  }
}

handleAsyncErrors();
```

Sample output:

```
Caught async error: Failed miserably
```

## Deep Dive
Error handling has been a cornerstone of programming since its inception. In TypeScript, which builds on JavaScript, error handling became more robust with the introduction of async/await in ECMAScript 2017. Before that, we often relied on callback functions and promises to handle errors in asynchronous code.

An alternative to `try/catch` in TypeScript is using error boundaries provided by frameworks like React. For server-side handling, we can use middleware in platforms like Express.js to centralize error management.

Implementation-wise, TypeScript doesn't have its own error handling mechanism but relies on JavaScript's. Custom error classes can extend the `Error` class to offer more descriptive error information.

## See Also
- [MDN on try/catch](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/try...catch)
- [Async/Await on MDN](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Asynchronous/Async_await)
- [Using Error Boundaries in React](https://reactjs.org/docs/error-boundaries.html)
- [Express.js Error Handling](https://expressjs.com/en/guide/error-handling.html)
