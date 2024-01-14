---
title:                "TypeScript recipe: Concatenating strings"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why

As a TypeScript programmer, you may have encountered the need to combine multiple strings together into one. This process, known as concatenation, allows you to create longer and more complex strings by joining smaller ones. In this blog post, we will explore the importance of concatenating strings and how to do it effectively in TypeScript.

## How To

Concatenating strings in TypeScript is a simple yet powerful tool in your coding arsenal. Let's take a look at some examples below using ```TypeScript``` code blocks:

```
// Example 1: Using the "+" operator
let firstName: string = "John";
let lastName: string = "Doe";

console.log(firstName + " " + lastName);
// Output: John Doe

// Example 2: Using the concat() method
let sentence: string = "Hello";
sentence = sentence.concat(" ", "world!");

console.log(sentence);
// Output: Hello world!

// Example 3: Using template literals
let num: number = 5;
let result: string = `The number is: ${num}`;

console.log(result);
// Output: The number is: 5
```

In the first example, we use the "+" operator to combine the two strings, "John" and "Doe". This operator acts as a concatenation operator in TypeScript, allowing you to add strings together. In the second example, we use the built-in ```concat()``` method to join the words "Hello" and "world!". Lastly, we use template literals - a feature introduced in ES6 - to create a string using a template and variable values.

## Deep Dive

Behind the scenes, concatenation works by taking multiple strings and merging them into one new string. In TypeScript, strings are immutable, meaning they cannot be changed once created. Therefore, when concatenating strings, a new string is created with the desired values. This is why we can reassign the variable in the second example and use template literals to create a new string.

Additionally, it is important to note that the ```concat()``` method and template literals support any number of arguments, allowing you to concatenate multiple strings at once. It is also possible to concatenate other data types, such as numbers and booleans, by converting them to strings first.

## See Also

To learn more about string concatenation in TypeScript, check out the official documentation: 
- https://www.typescriptlang.org/docs/handbook/2/template-literal-types.html#string-concatenation
- https://www.typescriptlang.org/docs/handbook/basic-types.html#string 

Happy coding!