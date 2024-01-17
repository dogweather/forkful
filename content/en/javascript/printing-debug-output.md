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

## What & Why?

Debug output, also known as debugging, is the process of displaying information during the execution of a program to help understand its behavior and track down any issues. It allows programmers to see the current state of the code and identify where errors may be occurring. Debugging is an essential tool for improving the functionality and reliability of code.

## How to:

To print debug output in JavaScript, we can use the console.log() function. This function takes in any value or variable and logs it to the console for us to see. Let's take a look at an example:

```javascript
let num1 = 5;
let num2 = 10;
console.log(num1 + num2); // prints 15 to the console
```

In this example, we declare two variables and use the console.log() function to print their sum to the console. This can be especially useful when trying to understand the outcome of certain operations or checking the value of a variable at a specific point in the code.

We can also use the console.time() and console.timeEnd() functions to measure the execution time of a specific portion of our code. Let's look at an example:

```javascript
console.time("MyLoop");
for (let i = 0; i < 1000000; i++) {
  // do some operation
}
console.timeEnd("MyLoop"); // prints the time it took to complete the loop to the console
```

## Deep Dive:

Debugging has been an important aspect of programming for decades. In the past, programmers would use manual code inspection or print statements to debug their code. But with the introduction of modern debuggers and tools, the process has become more efficient and effective.

Apart from console.log(), there are other alternatives for printing debug output in JavaScript, such as console.info(), console.warn(), and console.error(). These functions provide different levels of output and can be useful for organizing and prioritizing debug messages.

When using console.log(), we can also format our output by using string templates or passing multiple arguments to the function. This can make our debug messages more descriptive and helpful when trying to identify issues.

## See Also:

- [MDN Web Docs: Debugging JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/debugger)
- [YouTube: How to Debug JavaScript in Google Chrome](https://www.youtube.com/watch?v=w1sAvv2612M)
- [dev.to: 5 Console Methods You Didn’t Know Existed](https://dev.to/dcodeyt/5-console-methods-you-didn-t-know-existed-14c0)