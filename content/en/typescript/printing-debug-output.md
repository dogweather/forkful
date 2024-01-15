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

## Why

Printing debug output is a common practice in software development for troubleshooting and testing purposes. It allows developers to see the values of variables and the flow of their code, helping them to identify and fix any errors or bugs in their program.

## How To

To print debug output in TypeScript, you can simply use the `console.log()` method. This method takes in any number of arguments, which can be strings, variables, or even objects.

```TypeScript
// Printing a string
console.log("Hello world!");

// Printing a variable
let num = 5;
console.log(num);

// Printing an object
let person = {
    name: "John",
    age: 35
}
console.log(person);
```

The above code will print the following output in the console:

```
Hello world!
5
{ name: "John", age: 35 }
```

You can also format your debug output using string literals and template literals, which allows you to insert variables and expressions into a string.

```TypeScript
let name = "Lisa";
let age = 25;
console.log(`My name is ${name} and I am ${age} years old.`);
```

This will print the following output:

```
My name is Lisa and I am 25 years old.
```

## Deep Dive

In addition to `console.log()`, TypeScript also has other methods for printing debug output, such as `console.debug()` and `console.info()`. These methods are useful for differentiating between different types of debug messages.

You can also use the `console.trace()` method to print a stack trace, which displays the current call stack of your code. This is helpful for identifying which functions or methods are being called and in what order.

Lastly, you can use conditional statements with your debug output to only print certain messages if a certain condition is met. This can be done using the `console.assert()` method, which will only print the message if the condition evaluates to false.

## See Also

- [Console API in TypeScript](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-3.html#console)
- [Debugging Techniques in TypeScript](https://www.youtube.com/watch?v=_fRQd98MtSA)