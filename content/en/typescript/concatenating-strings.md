---
title:    "TypeScript recipe: Concatenating strings"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Why

Concatenating strings may seem like a simple task, but it is an essential part of coding in TypeScript. It allows us to combine separate pieces of text and variables into a single string, making our code more versatile and dynamic. In this blog post, we will explore the simple yet powerful concept of string concatenation and how it can improve our coding experience.

## How To

To concatenate strings in TypeScript, we use the "+" operator. Let's take a look at a simple example:

```TypeScript
let firstName: string = "John";
let lastName: string = "Doe";

let fullName: string = firstName + " " + lastName;
console.log(fullName); // Output: "John Doe"
```

In this example, we have two variables, `firstName` and `lastName`, containing strings. We then create a new variable, `fullName`, and assign it the concatenated string using the "+" operator. Finally, we use `console.log()` to log the result, which is the full name "John Doe".

We can also use string interpolation, denoted by the "$" symbol, to concatenate strings with variables. Let's see an example of this:

```TypeScript
let num: number = 42;

let message: string = `The answer to life, the universe, and everything is ${num}`;
console.log(message); // Output: "The answer to life, the universe, and everything is 42"
```

In this example, we use string interpolation to insert the value of the variable `num` into our string.

## Deep Dive

Under the hood, when we use the "+" operator to concatenate strings, TypeScript converts them into the `string` type. That is why we can use the "+" operator with other data types such as numbers, booleans, or objects. Let's see an example of this:

```TypeScript
let str: string = "Hello";

let newStr: string = str + 42 + true;
console.log(newStr); // Output: "Hello42true"
```

In this example, TypeScript automatically converts the number and boolean into strings before concatenating them with the existing string.

It is important to note that overly complex or nested concatenations can lead to performance issues in our code. In such cases, it is recommended to use the `join()` method or template literals for cleaner and more efficient concatenation.

## See Also

If you want to learn more about string concatenation in TypeScript, check out these helpful resources:
- [TypeScript Documentation on String Concatenation](https://www.typescriptlang.org/docs/handbook/2/template-literal-types.html#template-literal-types)
- [MDN Web Docs on String Concatenation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Addition_assignment)

Happy coding!