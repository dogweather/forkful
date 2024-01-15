---
title:                "Printing debug output"
html_title:           "Javascript recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why

Printing debug output is a useful tool for developers to troubleshoot and understand the inner workings of their code. It allows for visibility into the values of variables and the flow of execution, making it easier to identify and fix any errors.

## How To

To print debug output in Javascript, you can use the `console.log()` method. This method will output any parameters passed into it to the browser's console. Let's take a look at an example:

```Javascript
let num = 5;
console.log(num); // outputs 5 to the console
```

In this example, we declare a variable `num` and assign it the value of 5. We then use `console.log()` to print the value of `num` to the console.

You can also pass multiple parameters to be logged, separated by commas:

```Javascript
let name = "John";
let age = 25;
console.log(name, age); // outputs "John 25" to the console
```

In addition to logging values, you can also add in descriptive messages to help identify the output:

```Javascript
let x = 10;
console.log("The value of x is:", x); // outputs "The value of x is: 10" to the console
```

This is useful for keeping track of multiple variables and their values while debugging.

## Deep Dive

In addition to `console.log()`, there are a few other methods that can be used for printing debug output:

- `console.debug()`: Similar to `console.log()`, but used for less important or supplemental debugging information.
- `console.warn()`: Outputs a warning message to the console.
- `console.error()`: Outputs an error message to the console.

You can also use `console.table()` to print out data in a table format, making it easier to read and analyze.

Another useful tip is to use `console.group()` and `console.groupEnd()` to group related outputs and make them more organized. For example:

```Javascript
console.group("User Info");
console.log("Name: John");
console.log("Age: 25");
console.groupEnd();
```

This will group the information about the user together in the console, making it easier to distinguish from other logs.

## See Also

- [MDN web docs: console object](https://developer.mozilla.org/en-US/docs/Web/API/console)
- [The Ultimate Guide to JavaScript Debugging](https://www.freecodecamp.org/news/javascript-debugging-guide/)
- [Debugging Like a Boss with Browser DevTools](https://blog.logrocket.com/debugging-like-a-boss-with-browser-devtools/)