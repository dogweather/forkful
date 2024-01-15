---
title:                "Finding the length of a string"
html_title:           "TypeScript recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why

Knowing the length of a string is an important aspect of programming. It allows us to easily manipulate and validate user inputs, work with data structures, and more. In this article, we will explore how to find the length of a string in TypeScript and why it is important.

## How To

To find the length of a string in TypeScript, we can use the `length` property. Let's take a look at an example:

```TypeScript
let str: string = "Hello, world!";
console.log(str.length); // Output: 13
```

In this code, we have declared a string variable `str` and assigned it the value of "Hello, world!". Then, by accessing the `length` property, we get the number of characters in the string, which in this case is 13.

We can also use the `length` property to iterate through a string using a loop. For example:

```TypeScript
let name: string = "John";
for (let i = 0; i < name.length; i++) {
    console.log(name[i]); // Output: J o h n
}
```

Here, we are using a for loop to iterate through the string `name` and print each character. As the `length` property returns the number of characters in the string, we can use it as a condition for our loop to make sure we iterate through the entire string.

## Deep Dive

In TypeScript, the `length` property is available for both primitive strings and string objects. However, there is a slight difference between the two. For primitive strings, the `length` property is read-only, meaning we can access it but cannot change its value. For string objects, the `length` property is writable, which means we can modify its value.

It is also worth noting that the `length` property counts the number of UTF-16 code units in a string. This can be a bit confusing as some characters in JavaScript are represented by two code units (known as surrogate pairs). Therefore, the `length` property may not always accurately represent the actual number of visible characters in a string.

## See Also

- [TypeScript Official Documentation on Strings](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- [MDN Web Docs on String Length](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)