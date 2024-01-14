---
title:                "Javascript recipe: Writing to standard error"
programming_language: "Javascript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why

As a programmer, you have probably encountered errors in your code at some point. But have you ever wondered how those errors are displayed and what you can do with them? In this blog post, we will be discussing the importance of writing to standard error in Javascript and how it can be helpful in debugging your code.

## How To

To write to standard error in Javascript, we can use the `console.error()` method. This method takes in a string as an argument and outputs it to the standard error stream. Let's take a look at an example:

```Javascript
console.error("This is an error message");
```

The above code will output the following in your console:

`This is an error message`

With this method, you can easily print out specific error messages in your code, making it easier to identify and fix any issues.

But what if we want to display more information about the error, such as the line number or the file name? For this, we can use `console.error()` with the `Error` object. This object contains information about the error, including the line number and file name. Let's see how this works:

```Javascript
const num1 = 10;
const num2 = 0;

try {
  if (num2 === 0) {
    throw new Error("Division by zero");
  } else {
    console.log(num1 / num2);
  }
} catch (err) {
  console.error(err);
}
```

In the example above, we have a `try...catch` block that will catch any errors that occur within it. If an error is thrown, we use `console.error()` to print out the error message and the error object, which will give us information about where the error occurred. The output of this code would be:

`Error: Division by zero
  at <anonymous>:6:11`

Here, we can see that the error occurred on line 6 (where we attempted to divide `num1` by `num2`), and we also have the option to click on the line number and be taken directly to where the error occurred in our code.

## Deep Dive

Now that we understand how to write to standard error in Javascript, let's take a deeper dive into why this is important. Printing errors to the standard error stream is useful because it separates them from standard output, making it easier to distinguish between regular program output and potential errors. This can be especially helpful when dealing with larger, more complex code bases.

Additionally, writing to standard error allows us to capture and handle errors more efficiently. In our previous example, we used `try...catch` to catch any errors that occur within the block of code. This allows us to control how the error is handled, whether it be by displaying a custom error message or gracefully exiting the program.

## See Also

- [Error Object in Javascript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Error)
- [Console Methods in Javascript](https://developer.mozilla.org/en-US/docs/Web/API/Console#Methods)

Writing to standard error may seem like a small detail, but it can greatly improve our debugging process and make our code more efficient. So the next time you encounter an error in your code, remember that `console.error()` is your friend!