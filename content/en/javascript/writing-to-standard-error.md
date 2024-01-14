---
title:    "Javascript recipe: Writing to standard error"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Why

When writing code in JavaScript, it's important to have a way to handle errors in case something goes wrong. By writing to standard error, developers can efficiently debug their code and identify any potential issues. It's a crucial part of the programming process and can save a lot of time and frustration in the long run.

## How To

To write to standard error in JavaScript, we can use the `console.error()` function. This function takes in a string or any type of data as its argument and outputs it to the standard error stream. Let's see an example:

```Javascript
let num = 10;
console.error("The value of num is: " + num);
```

Running this code will produce the following output:

```
The value of num is: 10
```

We can also use template literals to format our output in a more dynamic way. Let's take a look at another example:

```Javascript
let name = "John";
let age = 28;

console.error(`Hello, my name is ${name} and I am ${age} years old.`);
```

The output will be:

```
Hello, my name is John and I am 28 years old.
```

Writing to standard error is a simple and effective way to handle errors in JavaScript. It allows developers to quickly identify and fix any issues in their code.

## Deep Dive

Standard error is one of the three standard streams in a computer's input/output system. It is typically used to output error messages and any other diagnostic information. In contrast, standard output (often known as `console.log`) is used for general output, while standard input is used for receiving input from the user.

In JavaScript, we can also use `throw` statements to explicitly throw an error to standard error. This is useful when we want to handle specific errors with custom messages. For example:

```Javascript
function divide(x, y) {
  if (y === 0) {
    throw new Error("Cannot divide by zero!");
  }

  return x / y;
}

console.error(divide(10, 0));
```

The output will be:

```
Error: Cannot divide by zero!
    at divide (<anonymous>:3:11)
    at <anonymous>:7:13
```

Using `console.error` in conjunction with `throw` statements can provide developers with more control and flexibility when handling errors in their code.

## See Also

- [Node.js documentation on console.error()](https://nodejs.org/api/console.html#console_console_error_data_args)
- [MDN web docs on throw statements](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/throw)