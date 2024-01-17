---
title:                "Printing debug output"
html_title:           "TypeScript recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?

Printing debug output is the act of displaying informational messages in the console during the execution of a program. Programmers use this technique to track and analyze the behavior of their code, helping them identify and fix errors or unexpected results.

## How to:

To print debug output in TypeScript, you can use the `console.log()` method to display messages in the console. You can pass any data type, such as strings, numbers, or objects, as arguments to the method.

```TypeScript
let name = "John";
console.log(`Hello ${name}!`);
```
Output: `Hello John!`

You can also use multiple parameters in the `console.log()` method to display different messages or variables on the same line.

```TypeScript
let num1 = 10;
let num2 = 20;
console.log("First number:", num1, "Second number:", num2);
```
Output: `First number: 10 Second number: 20`

## Deep Dive:

In the early days of programming, developers used physical printers to produce debug output. This was a time-consuming and tedious process, so the introduction of console-based debug output greatly improved the efficiency of debugging.

An alternative to printing debug output in the console is using a debugger tool. Debuggers allow you to set breakpoints in your code, pause the execution, and inspect the values of variables at that specific point. This can be more useful when dealing with complex code.

In TypeScript, the `console.log()` method is implemented using the `console` global object, which is provided by the web browser or Node.js environment. This means that it may have some minor differences in implementation depending on the environment.

## See Also:

- [TypeScript Documentation on console debugging](https://www.typescriptlang.org/docs/handbook/integrating-with-build-tools.html#debugging-typescript)
- [Debugging in TypeScript Using Visual Studio Code](https://code.visualstudio.com/docs/typescript/typescript-debugging)