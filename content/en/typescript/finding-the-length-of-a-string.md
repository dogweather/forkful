---
title:    "TypeScript recipe: Finding the length of a string"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Why

As a programmer, you might often come across situations where you need to manipulate strings. One common task is finding the length of a string. This might seem like a basic operation, but it is a fundamental part of string manipulation and understanding it can help improve your overall programming skills.

## How To

Finding the length of a string in TypeScript is a simple task. First, we need to declare a variable and assign a string value to it. Then, we can use the `length` property to get the length of the string.

```TypeScript
let myString = "Hello World";
console.log(myString.length); // Outputs: 11
```

In the above example, we have declared a variable `myString` and assigned it a value of "Hello World". Then, we use the `length` property to get the length of the string, which is 11. This property counts the number of characters in the string, including spaces and special characters.

We can also use the `length` property on an empty string, and it will return 0.

```TypeScript
let emptyString = "";
console.log(emptyString.length); // Outputs: 0
```

Additionally, the `length` property can also be used on arrays, where it will return the number of elements in the array.

```TypeScript
let myArray = [1, 2, 3, 4, 5];
console.log(myArray.length); // Outputs: 5
```

## Deep Dive

Under the hood, the `length` property works by iterating through each character in the string and counting them. This is why the `length` property is always available on strings, as it is a built-in functionality.

One thing to note is that the `length` property returns the actual length of the string, not the index of the last character. This means that if your string has 11 characters, the last character's index would be 10, but the `length` property will return 11.

Also, it is important to remember that the `length` property only returns the number of characters, not the memory size of the string.

## See Also

- [MDN Web Docs - String length](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [TypeScript Official Documentation - String.length](https://www.typescriptlang.org/docs/handbook/strings.html#string-length)
- [W3Schools - JavaScript String length Property](https://www.w3schools.com/jsref/jsref_length_string.asp)