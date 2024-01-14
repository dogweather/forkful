---
title:                "TypeScript recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why

Converting a string to lower case is a common task in programming, especially when dealing with user input. It allows for consistency in data and easier comparison between strings. In this blog post, we will explore how to convert a string to lower case using TypeScript.

## How To

To convert a string to lower case in TypeScript, we can use the `toLowerCase()` method. This method returns a new string with all characters converted to lower case. Let's take a look at an example:

```TypeScript
const str = "Hello World";
console.log(str.toLowerCase());
// Output: hello world
```

In the example above, we declare a constant `str` with the value of "Hello World". Then, we use the `toLowerCase()` method to convert it to lower case and print the result to the console. 

But what if we want to convert only a specific portion of the string to lower case? We can use the `slice()` method to first get the desired portion and then apply the `toLowerCase()` method on it. Here's an example:

```TypeScript
const str = "Hello World";
console.log(str.slice(0, 5).toLowerCase());
// Output: hello
```

Here, we first use the `slice()` method to get the characters from index 0 to 4 (as `slice()` excludes the end index) and then apply the `toLowerCase()` method on the result. This will convert only the first five letters to lower case.

## Deep Dive

Under the hood, the `toLowerCase()` method uses the Unicode Standard for case mapping. This means that it converts any characters that have uppercase and lowercase versions to their lowercase form according to the Unicode table. For example, the German letter "Ö" will be converted to "ö" in lower case.

It's important to note that the `toLowerCase()` method does not mutate the original string. Instead, it returns a new string with the lower case conversion. This is because strings in TypeScript are immutable, meaning they cannot be changed. This also means that we need to assign the result of the conversion to a new variable or use it directly in our code.

## See Also

- [String.prototype.toLowerCase() - MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [Unicode Standard](https://www.unicode.org/standard/standard.html)
- [TypeScript Strings](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)