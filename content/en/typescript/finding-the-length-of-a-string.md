---
title:                "TypeScript recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why

As programmers, we often come across the need to manipulate and work with strings in our code. One common task is finding the length of a string. This simple but useful operation can be helpful in various scenarios such as validating input and formatting output. In this blog post, we will explore how to find the length of a string in TypeScript.

## How To

To find the length of a string in TypeScript, we can use the `length` property. Let's take a look at an example:

```TypeScript
let str: string = "Hello, world!";
let length: number = str.length;

console.log(`The length of '${str}' is ${length}`); // Output: The length of 'Hello, world!' is 13
```

In the above code, we first declare a variable `str` of type `string` and assign it a value of "Hello, world!". Then, we use the `length` property to get the length of the string and store it in another variable `length`. Finally, we use string interpolation to display the string and its length in the console.

We can also directly use the `length` property on a string literal, as shown below:

```TypeScript
console.log(`The length of 'Hello, world!' is ${"Hello, world!".length}`); // Output: The length of 'Hello, world!' is 13
```

If we want to find the length of a multi-line string, we need to consider the line breaks. In TypeScript, line breaks are represented by the special character `\n`. So, to get the correct length, we can add the number of line breaks to the `length` property, as shown in the example below:

```TypeScript
let multilineStr: string = "Hello,\nworld!";
let length: number = multilineStr.length + 1; // Adding 1 for the line break

console.log(`The length of '${multilineStr}' is ${length}`); // Output: The length of 'Hello,
// world!' is 14
```

## Deep Dive

When we call the `length` property on a string, it internally uses the `string.length` property, which is a JavaScript function. This function returns the actual number of UTF-16 code units in the string, rather than the number of characters. This is because JavaScript/TypeScript represents strings as a sequence of 16-bit code units, while some characters may require more than one code unit to be represented.

This can be seen in the following example:

```TypeScript
let emoji: string = "🚀";
console.log(`The length of '${emoji}' is ${emoji.length}`); // Output: The length of '🚀' is 2
```

In this case, although the string "🚀" has only one character, it occupies two code units in memory. Hence, the `length` property returns 2 instead of 1.

## See Also

- [TypeScript Official Documentation on Strings](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- [MDN Web Docs on String.prototype.length](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [Blog Post on Emojis and UTF-16 in JavaScript](https://medium.com/@iaincollins/how-emojis-work-in-javascript-f2621588a3f8)