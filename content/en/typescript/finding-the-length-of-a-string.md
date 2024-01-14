---
title:    "TypeScript recipe: Finding the length of a string"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why

As programmers, we often encounter the need to manipulate strings in our code. One common task is to find the length of a string. Understanding how to do this in TypeScript can save us time and make our code more efficient.

## How To

To find the length of a string in TypeScript, we can use the built-in `length` property. This property returns the number of characters in the string, including white space. Let's take a look at an example:

```TypeScript
let string = "Hello World";
console.log(string.length);
```

This will output `11` since there are 11 characters in the string "Hello World". We can also use this property on an empty string `""` which will return a length of `0`.

If we want to find the length of a string with special characters, we need to use the `length` property in combination with the `unicode` property. The Unicode value of a character is its numerical representation. For example, the Unicode value for the letter "a" is `97`. Here's an example of how we can find the length of a string with special characters:

```TypeScript
let string = "Héłłø Wörld";
console.log(string.length); // Outputs 12
console.log(string.unicode.length); // Outputs 13
```

Here, the first `console.log` outputs `12` since there are 12 characters in the string. The second `console.log` outputs `13` since there are two special characters with Unicode values.

## Deep Dive

The `length` property may seem simple, but there is actually a lot going on behind the scenes. When we access the `length` property, TypeScript is essentially calling the `length()` method on the string object. This method calculates the length by iterating through each character in the string and counting them.

It's important to note that the `length` property only counts actual characters in the string, not the index positions. In other words, it does not count the index of the last character, but rather the number of characters from the first index to the last.

## See Also

- [MDN web docs: String length](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [TypeScript documentation: Strings](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)