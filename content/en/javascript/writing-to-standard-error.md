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

## Why

Writing to standard error, also known as stderr, is an essential skill for any programmer. It allows you to display error messages and troubleshoot code, making the debugging process easier and more efficient.

## How To

To write to standard error in Javascript, we use the `console.error()` method. This method takes in one or more arguments and logs them to the stderr stream. Let's take a look at a simple example:

```Javascript
console.error("Oops, something went wrong!");
```

This will result in the following output in the console:

```
Oops, something went wrong!
```

We can also pass in multiple arguments to `console.error()` by separating them with commas:

```Javascript
let num1 = 5;
let num2 = 0;

console.error("Cannot divide", num1, "by", num2);
```

The output will be:

```
Cannot divide 5 by 0
```

Note that unlike `console.log()`, which writes to standard output or stdout, `console.error()` writes to stderr. This means that the error messages will be displayed with a different color in the console, making them stand out more.

## Deep Dive

The `console.error()` method can take in any type of data as arguments, including strings, numbers, objects, and arrays. It can also take in template literals, making it a versatile tool for displaying error messages.

One common use case for writing to stderr is error handling. For example, we can use `console.error()` in a try-catch block to catch and log any errors that occur in our code:

```Javascript
try {
  // Some code that may throw an error
} catch(err) {
  console.error("An error occurred:", err);
}
```

This allows us to see the details of the error in the console and helps us troubleshoot the issue.

Another useful feature of `console.error()` is that it supports string formatting. We can use format specifiers to display the values of variables within our error message:

```Javascript
let errorMsg = "%s is not a valid age for %s";

console.error(errorMsg, 15, "voting");
```

The output will be:

```
15 is not a valid age for voting
```

For a list of all the available format specifiers, you can refer to the [official documentation](https://nodejs.org/api/console.html#console_console_error_data_args).

## See Also

- [Node.js Console documentation](https://nodejs.org/api/console.html)
- [Debugging with Node.js](https://nodejs.org/en/docs/guides/debugging-getting-started/)
- [Understanding stderr, stdout, and stdio](https://www.beyondlinux.com/3-ways-to-redirect-error-messages-to-a-file-in-linux/)