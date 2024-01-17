---
title:                "Writing to standard error"
html_title:           "Javascript recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?

Writing to standard error in Javascript means sending error messages to the developer's console instead of the regular output destination. Programmers do this to provide a separate and distinct channel for error messages, making it easier to identify and troubleshoot issues in their code.

## How to:

To write to standard error in Javascript, use the "console.error()" function. This function takes in a string or object as an argument and prints it to the console in red, distinguishing it from regular console logs.

```
// Example code
console.error("Something went wrong!"); // Outputs "Something went wrong!" in red
```

In the following example, an object is passed as an argument and the console prints out the object's properties:

```
// Example code
let errorObj = {
  errorCode: 404,
  errorMessage: "Page not found"
};
console.error(errorObj); // Outputs "{ errorCode: 404, errorMessage: "Page not found" }" in red
```

## Deep Dive:

### Historical Context:

Writing to standard error is a common practice in programming, especially in languages like C and Java. It allows developers to separate error messages from normal output, making debugging and error handling more efficient. In Javascript, this feature was introduced in ES5.

### Alternatives:

An alternative to using console.error() is to throw an error using the "throw" keyword. Throwing an error will stop the execution of the code and display the error message in the console. However, this method is usually used for critical errors that require the program to stop running.

### Implementation Details:

Under the hood, console.error() sends the error message to the standard error stream, which is usually associated with the developer's console. However, the exact implementation may differ depending on the browser or environment where the code is run.

## See Also:

- [console.error() - MDN web docs](https://developer.mozilla.org/en-US/docs/Web/API/Console/error)
- [Throw statement - MDN web docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/throw)