---
title:    "Javascript recipe: Printing debug output"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why
Debugging is an essential part of programming, and sometimes it can be challenging to identify and fix errors in your code. One useful technique for debugging is to print out debug output, which can help you understand the flow of your code and identify any issues. In this blog post, we will explore how to print debug output in JavaScript and why it is a valuable tool for every programmer.

## How To
To print out debug output in JavaScript, we can use the `console.log()` function. This function takes in a value or expression and prints it to the console. Let's take a look at an example:

```Javascript
let num1 = 5;
let num2 = 10;
console.log(num1 + num2);
```

In the above code, we declare two variables, `num1` and `num2`, and then we use `console.log()` to print out their sum, which is 15. We can also use this function to print out the value of a variable or an expression at a particular point in our code. For example:

```Javascript
let name = "John";
console.log(`Hello, my name is ${name}`);
```
The output of the above code will be: `Hello, my name is John`.

By printing out these debug outputs, we can track the values of our variables and see how they change as our code executes. This can be particularly helpful when dealing with complex functions or loops, as we can see the values of our variables at each iteration.

## Deep Dive
While `console.log()` is the most commonly used method for printing debug output, there are a few other useful options available in JavaScript. For example, we can use `console.error()` to print out specific error messages, and `console.warn()` to display warnings.

Another helpful tool is the `debugger` statement, which allows us to pause our code's execution at a particular point and inspect the values of our variables in the debugger console. This can be more useful than continuously printing out values, especially when dealing with large amounts of data.

Additionally, we can also use browser developer tools to print out more in-depth debug output, such as network requests, DOM elements, and more. These tools are invaluable for debugging complex web applications and can help us identify and fix issues quickly.

See Also
- [console.log() documentation](https://developer.mozilla.org/en-US/docs/Web/API/Console/log)
- [debugger statement documentation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/debugger)
- [Chrome Developer Tools](https://developers.google.com/web/tools/chrome-devtools)
- [Firefox Developer Tools](https://developer.mozilla.org/en-US/docs/Tools)