---
title:                "TypeScript recipe: Writing to standard error"
programming_language: "TypeScript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why

Writing to standard error, also known as stderr, is a common practice in programming that allows for error handling and debugging. By writing to stderr instead of the usual standard output (stdout), developers can differentiate between regular program output and error messages, making it easier to identify and troubleshoot issues.

## How To

To start writing to standard error in TypeScript, we can use the built-in `console.error()` method. Let's take a look at an example:

```TypeScript
const num1 = 10;
const num2 = 0;

if (num2 === 0) {
  console.error("Error: Cannot divide by zero!");
} else {
  const result = num1 / num2;
  console.log(`Result: ${result}`);
}
```

In this code snippet, we first define two variables, `num1` and `num2`, with `num2` being assigned a value of 0. Then, we use an `if` statement to check if `num2` is equal to 0. If it is, we use `console.error()` to print out an error message. Otherwise, we calculate the result and print it out using `console.log()`, which writes to stdout.

The output of this code will be:

```bash
Error: Cannot divide by zero!
```

As we can see, the error message was printed to stderr, while the result was printed to stdout.

## Deep Dive

In TypeScript, writing to stderr is useful not only for handling errors, but also for logging important information during debugging. Additionally, we can use `console.error()` to log objects and their properties, making it easier to inspect them and find any potential issues.

Another useful feature is that we can redirect stderr to a file for easier error tracking and analysis. This can be done by appending `2> error.log` to the end of the command when running the TypeScript file. This will redirect all stderr output to the specified `error.log` file.

## See Also

- [Official TypeScript Documentation on `console.error()`](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-3.html#improved-downlevel-includes)
- [Node.js Documentation on `process.stderr`](https://nodejs.org/api/process.html#process_process_stderr)
- [Stack Overflow Discussion on Redirecting stderr to a File](https://stackoverflow.com/questions/7522925/how-to-redirect-both-stdout-and-stderr-to-a-file)