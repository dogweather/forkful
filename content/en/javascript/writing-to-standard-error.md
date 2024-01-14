---
title:                "Javascript recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why

When writing programs in Javascript, it is common to encounter errors or bugs. One way to handle these errors is by using the `console.error()` method to write them to the standard error stream. This allows developers to easily identify and debug issues in their code.

## How To

Writing to standard error in Javascript is a simple process. First, we need to identify the error or the piece of code that we want to print to the standard error stream. For example, if we have a variable called `num` that we want to print to standard error, our code would look like this:

```Javascript
 let num = 10;
 console.error(num);
```

Running this code would result in the number 10 being printed to the standard error stream. This can be very useful when trying to identify the source of an error in a larger codebase.

## Deep Dive

The `console.error()` method works by printing the given arguments to the standard error stream, which is typically the console for most Javascript environments. This can be especially helpful when trying to debug complex errors that may not be visible in the standard output.

Additionally, the `console.error()` method supports multiple arguments, so you can also print out multiple pieces of information to the standard error stream at once. For example, our previous code could be rewritten as:

```Javascript
 console.error("The value of num is:", 10);
```

This would result in the following output:

```Javascript
 The value of num is: 10
```

By using this method, developers can easily track down errors and log important information without cluttering up their code with repetitive `console.log()` statements.

## See Also

- [MDN web docs on console.error()](https://developer.mozilla.org/en-US/docs/Web/API/Console/error)
- [W3Schools guide on Javascript Errors](https://www.w3schools.com/js/js_errors.asp)
- [Codecademy tutorial on debugging in Javascript](https://www.codecademy.com/articles/debugging-javascript)