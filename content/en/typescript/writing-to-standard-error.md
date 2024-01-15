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

## Why ##

Writing to standard error is an important aspect of programming in TypeScript as it allows developers to log errors and debug their code efficiently. It provides valuable information that can help identify and fix bugs, leading to more robust and reliable code.

## How To ##

To write to standard error in TypeScript, you can use the `console.error()` method. This method takes in a message as an argument and outputs it to the standard error stream. Let's look at an example:

```TypeScript
console.error('Something went wrong!');
```

The above code will output the message "Something went wrong!" to the standard error stream. This message will be displayed in the console, along with the line number and file name where the error occurred.

You can also pass in variables or objects as arguments to the `console.error()` method. This is helpful when you need to log the current state of your code. Let's see how it works:

```TypeScript
let num = 5;
console.error('The value of num is ', num);
```

The output of the above code will be "The value of num is 5". This can be particularly useful when troubleshooting errors in complex code.

## Deep Dive ##

In TypeScript, there are three streams for displaying output: standard output, standard error, and standard input. Standard output is the default stream used by `console.log()` method and can be redirected to a file or external system. Standard error, on the other hand, is the stream specifically used for displaying error messages.

By writing to standard error, developers can separate the error messages from the standard output, making it easier to identify and debug issues. This is especially useful for larger projects where there may be multiple developers working on different parts of the code.

It is important to note that the `console.error()` method does not actually throw an error, it simply outputs a message to the standard error stream. So, this method should be used for logging errors and not for handling exceptions.

## See Also ##

- [TypeScript Official Documentation](https://www.typescriptlang.org/docs/)
- [Console Methods in TypeScript](https://www.typescripttutorial.net/typescript-tutorial/console-methods/)
- [Debugging in TypeScript](https://code.visualstudio.com/docs/typescript/typescript-debugging)