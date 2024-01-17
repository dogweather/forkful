---
title:                "Writing to standard error"
html_title:           "TypeScript recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?

Writing to standard error is a way for developers to display error messages or debug information in their code. This is useful for troubleshooting and identifying problems in a program.

## How to:
To write to standard error in TypeScript, you can use the built-in `console.error()` method. This method takes in an error message or any other string as its argument.

```TypeScript
console.error("This is an error message"); // Output: This is an error message
```

You can also use string interpolation to include variables or other dynamic information in your error message.

```TypeScript
let num = 5;
console.error(`The value of num is ${num}`); // Output: The value of num is 5
```

## Deep Dive:
Writing to standard error has been a common practice in programming for many years. Before the introduction of standardized error handling methods, developers would often write error messages to a specific output stream, such as standard error, to differentiate them from regular program output.

In addition to `console.error()`, TypeScript also offers other console methods for writing to standard output. These include `console.log()` for general logging, `console.warn()` for warning messages, and `console.debug()` for debugging messages.

Another alternative to using `console.error()` is throwing an error using the `throw` keyword. This allows for more specific and customized error messages. However, this should only be used for critical errors that require the program to stop.

## See Also:
- [Official TypeScript Documentation on console methods](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-4.html#prioritized-overloads)
- [Article on the difference between console.error() and throw](https://spin.atomicobject.com/2011/05/24/the-difference-between-console-log-and-console-error/)
- [Blog post on debugging with console methods in TypeScript](https://blog.logrocket.com/debugging-typescript-with-the-console-api/)