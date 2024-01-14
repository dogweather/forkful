---
title:                "TypeScript recipe: Printing debug output"
programming_language: "TypeScript"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why

Have you ever found yourself struggling to understand what's going on in your code? Debugging can be a difficult and time-consuming task, especially when working with large and complex codebases. One helpful tool for debugging is printing debug output. By printing out specific values or messages in your code, you can gain a better understanding of what is happening at each step and identify potential issues. In this blog post, we'll explore how to use TypeScript to print debug output in your code.

## How To

To print debug output in TypeScript, we can use the `console.log()` function. This function allows us to log any value or message to the console, which can be viewed in the browser's developer tools. Let's take a look at a simple example:

```TypeScript
let num1 = 5;
let num2 = 10;

console.log(num1 + num2);
```

Output:
```
15
```

As you can see, the `console.log()` function prints out the result of adding `num1` and `num2`, which can be helpful in understanding the functionality of our code. We can also print out messages or the values of variables at different points in our code to track their values. For example:

```TypeScript
let num1 = 5;
let num2 = 10;

console.log("The value of num1 is: " + num1);
console.log("The value of num2 is: " + num2);

console.log(num1 + num2);
```

Output:
```
The value of num1 is: 5
The value of num2 is: 10
15
```

By logging out the values of `num1` and `num2` before the calculation, we can confirm that our variables have the correct values. This can be especially helpful when debugging complex conditions or loops.

## Deep Dive

In addition to the basic `console.log()` function, TypeScript also offers other debugging tools such as `console.error()` and `console.warn()`. These functions allow us to print out error and warning messages to help identify issues in our code. We can also use placeholders and string formatting to log multiple values or formatted messages, as shown in the example below:

```TypeScript
let name = "John";
let age = 30;

console.log("My name is %s and I am %d years old.", name, age);
```

Output:
```
My name is John and I am 30 years old.
```

For more complex debugging, we can also use the TypeScript debugger to step through our code line by line, set breakpoints, and inspect variables. This can be a powerful tool for understanding the flow of our code and pinpointing any errors.

## See Also

For more information on debugging in TypeScript, check out these helpful resources:

- [Official TypeScript documentation on debugging](https://www.typescriptlang.org/docs/handbook/debugging.html)
- [Debugging TypeScript in Visual Studio Code](https://code.visualstudio.com/docs/typescript/typescript-debugging)
- [Debugging TypeScript with Chrome DevTools](https://medium.com/@knistopher/debugging-typescript-with-chrome-devtools-592a675c7cdf)