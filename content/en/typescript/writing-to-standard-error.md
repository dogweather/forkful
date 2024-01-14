---
title:    "TypeScript recipe: Writing to standard error"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Why

As a programmer, it is important to have knowledge of how to write to standard error. This allows you to effectively handle and troubleshoot errors in your code, providing a better user experience for your application.

## How To

Writing to standard error in TypeScript is a simple process. First, we need to import the `console` module:

```TypeScript
import console from 'console';
```

Next, we can use the `console.error()` method to write to standard error. Let's say we have a variable `input` that contains a user's input. We can use the following code to check if the input is a number and write an error message if it is not:

```TypeScript
if(isNaN(input)) {
    console.error("Please enter a valid number");
}
```

The `console.error()` method will output the given message to the standard error stream, which can then be seen in the console or terminal when running the program.

## Deep Dive

When writing to standard error, it is important to understand the difference between writing to standard output (the `console.log()` method) and standard error. The main difference is that output messages are typically for regular program output, while error messages are for handling unexpected or critical events.

Furthermore, standard error has a different behavior than standard output when it comes to redirection. Output messages can be redirected to a file, but error messages will still be displayed in the console. This can be useful for easily identifying and troubleshooting errors in your code.

It is also worth noting that the `console.error()` method has an optional second argument, which accepts an object containing additional information to be displayed with the error message. This can be helpful for providing more context to the error.

## See Also

- [Node.js documentation on console module](https://nodejs.org/api/console.html)
- [W3Schools article on console.error()](https://www.w3schools.com/nodejs/met_console_error.asp)
- [GeeksforGeeks article on writing to standard error in TypeScript](https://www.geeksforgeeks.org/writing-to-standard-error-with-console-in-typescript/)