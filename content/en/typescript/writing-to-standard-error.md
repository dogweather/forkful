---
title:    "TypeScript recipe: Writing to standard error"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why

As a developer, one may often encounter errors and bugs while writing code. These errors can sometimes be difficult to track down and debug, especially in larger codebases. Writing to standard error can be a useful tool in providing more detailed information about these errors and how to fix them.

## How To

To write to standard error in TypeScript, we can use the `console.error` method. This method takes in a string or object as an argument and prints it to the standard error stream. Let's look at an example below:

```TypeScript
const num1 = 10;
const num2 = "five";

if (typeof num1 === "string") {
  console.error("num1 is not a number.");
}

if (typeof num2 === "number") {
  console.log(`5 + ${num2} = ${5 + num2}`);
} else {
  console.error("num2 is not a number.");
}
```

In the code above, we have two variables, `num1` and `num2`. We use the `typeof` operator to check the data type of each variable. If the data type is not what we expect, we can use `console.error` to print a message to the error stream. Running this code will produce the following output:

```bash
num1 is not a number.
num2 is not a number.
```

This makes it clear to the developer which variables are causing errors and can help in identifying the root cause of the issue.

## Deep Dive

Standard error (also known as "stderr") is a stream where a program can write error messages. This stream is separate from the standard output (stdout) stream, which is used for normal program output. In TypeScript, we can access the standard error stream through the `console.error` method.

One thing to note is that the messages written to standard error are typically displayed in red in most terminals, making it easier to distinguish them from regular program output. Additionally, the messages written to standard error are not buffered and will be immediately printed to the terminal.

It is best practice to use `console.error` for error messages and `console.log` for regular output. This helps in keeping our code organized and makes it easier to troubleshoot issues.

## See Also

- [TypeScript Official Documentation](https://www.typescriptlang.org/docs/home.html)
- [Writing to Standard Error in Node.js](https://www.w3schools.com/nodejs/nodejs_errors.asp)
- [Console Methods in TypeScript](https://blog.bitsrc.io/console-methods-in-javascript-7158b1c18c9c)