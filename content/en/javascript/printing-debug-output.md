---
title:    "Javascript recipe: Printing debug output"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Why 

Printing debug output, also known as console logging, is a useful tool for debugging and troubleshooting in any programming language. It allows developers to see the values of variables and the flow of their code, helping to identify and fix issues in their code.

## How To 

To print debug output in Javascript, we can simply use the `console.log()` function. Here's an example of how we can use it:

```Javascript
let x = 5;
let y = 10;

console.log("The value of x is", x); //Output: The value of x is 5
console.log("The value of y is", y); //Output: The value of y is 10
```

The console log accepts multiple arguments, so we can also print out the result of an expression:

```Javascript
let z = x + y;
console.log("The value of z is", z); //Output: The value of z is 15
```

We can also use string interpolation to print out variables inside a string:

```Javascript
let name = "John";

console.log(`Hello ${name}, welcome to my blog!`); //Output: Hello John, welcome to my blog!
```

## Deep Dive 

There are a few other methods available for console logging in Javascript, such as `console.warn()` and `console.error()` which can be used to log warnings and errors respectively. There's also `console.table()` which displays data in a table format.

One very useful feature of console logging is the ability to use different levels of logging, such as `console.debug()` and `console.info()`. This allows us to filter our debug output and only see the relevant information for the task we're working on.

Another useful tip is to use `console.assert()` to check if a condition is true, and if not, it will display an error message with the location of the error.

## See Also 

- [MDN Web Docs - Console.log()](https://developer.mozilla.org/en-US/docs/Web/API/Console/log)
- [Javascript.info - Debugging in Chrome](https://javascript.info/debugging-chrome)
- [FreeCodeCamp - How to use console.log() like a pro](https://www.freecodecamp.org/news/how-to-use-console-log-like-a-pro/)