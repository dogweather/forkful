---
title:                "Handling errors"
aliases:
- /en/javascript/handling-errors/
date:                  2024-01-21T21:20:11.687655-07:00
model:                 gpt-4-1106-preview
simple_title:         "Handling errors"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/handling-errors.md"
---

{{< edit_this_page >}}

## What & Why?

Error handling is how you manage when things go sideways in your code. It's key because it helps your programs fail gracefully and instructs users clearly, instead of just crashing and burning.

## How to:

Here's the classic `try-catch` block:

```javascript
try {
  // Code that might throw an error
  let result = potentiallyRiskyOperation();
  console.log('Success:', result);
} catch (error) {
  // What to do if an error is thrown
  console.error('Oops:', error.message);
}
```

Sample output when no error occurs:
```
Success: 42
```

And when there's an error:
```
Oops: Something went wrong
```

For asynchronous code, where promises are involved, use `try-catch` in an `async` function:

```javascript
async function fetchData() {
  try {
    let data = await fetch('https://api.example.com/data');
    console.log('Data fetched:', data);
  } catch (error) {
    console.error('Error fetching data:', error.message);
  }
}

fetchData();
```

## Deep Dive

Error handling in JavaScript has evolved. Back in the day (ES3, circa 1999), we just had the `try-catch` block. Not super flexible, but it did the job.

ES6 (2015) introduced Promises and gave us `.then()` and `.catch()`, allowing us to handle asynchronous errors more gracefully.

```javascript
fetch('https://api.example.com/data')
  .then(data => console.log('Data fetched:', data))
  .catch(error => console.error('Error fetching data:', error.message));
```

As for implementation details, when an error is thrown, JavaScript engines create an `Error` object with useful properties like `message` and `stack`. You can also make custom error types by extending the `Error` class – handy for more complex apps.

Alternatives? You could ignore error handling (bad idea), use callbacks with error-first parameters (hello, Node.js style), or get fancier with libraries and frameworks that offer their takes.

## See Also

For more on error handling:

- MDN on try-catch: [MDN try...catch](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/try...catch)
- Async/Await: [MDN async function](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/async_function)
- A guide to Promises: [MDN Promises](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise)
- Creating and throwing custom errors: [MDN Error](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Error)
