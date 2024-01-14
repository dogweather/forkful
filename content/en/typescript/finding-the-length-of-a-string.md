---
title:                "TypeScript recipe: Finding the length of a string"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why

When working on a programming project, you may come across a scenario where you need to find the length of a string. This can be for various reasons such as input validation, string manipulation or data analysis. Knowing how to find the length of a string in TypeScript is an important skill to have as it can greatly improve your ability to work with strings in your code.

## How To

To find the length of a string in TypeScript, we can use the built-in `length` property. This property returns the number of characters in the string and can be accessed through dot notation.

```TypeScript
let str: string = "Hello World";
console.log(str.length); //Output: 11
```

We can also use the type assertion operator to explicitly declare the type of a variable as `string` and then access the `length` property.

```TypeScript
let str = "Hello World" as string;
console.log(str.length); //Output: 11
```

Another way is to use the `toString()` method to convert the variable to a string, which will then allow us to access the `length` property.

```TypeScript
let str = 42;
console.log(str.toString().length); //Output: 2
```

It's important to note that the `length` property only counts the characters in a string, not including spaces or special characters. To include spaces and special characters, we can use the `split()` method to separate each character and then get the length of the resulting array.

```TypeScript
let str = "Hello World";
console.log(str.split("").length); //Output: 11
```

## Deep Dive

Under the hood, strings in TypeScript are objects and the `length` property is a method that calculates the number of characters in the string. This means that when we access the `length` property, it actually invokes this method to return the length value.

It's also worth mentioning that the return type of the `length` property is a `number`, not a `string` like the variable it is being accessed from. This can be useful when performing numerical operations on the string length.

## See Also

Here are some additional resources to learn more about finding the length of a string in TypeScript:

- [Official TypeScript documentation on strings](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- [MDN web docs on string length](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [TypeScript string data type](https://www.geeksforgeeks.org/typescript-string-data-type/)