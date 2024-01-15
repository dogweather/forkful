---
title:                "Extracting substrings"
html_title:           "TypeScript recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why

Substring extraction is a useful technique in programming for manipulating and extracting specific parts of a string. This can be useful for a variety of tasks, such as data processing, text parsing, and creating more dynamic and customizable outputs.

## How To

```TypeScript
// The basic syntax for extracting substrings in TypeScript is as follows:

let myString: string = "Hello World";

// To extract a substring, we can use the substring() method, which takes in two parameters: the starting index and the ending index.

myString.substring(0, 5); //returns "Hello"

// We can also use the slice() method, which also takes in two parameters, but instead of the ending index, it takes in the desired length of the substring.

myString.slice(6, 11); // returns "World"

// We can also use the substr() method, which takes in two parameters, the starting index and the desired length of the substring.

myString.substr(6, 5); // returns "World"
```

These methods can also be combined with other string manipulation methods, such as toUpperCase() or toLowerCase(), for even more flexibility in extracting substrings.

```TypeScript
// We can also use a negative index to start from the end of the string.

myString.substring(-6); // returns "World"

// We can also use these methods to extract substrings from arrays.

let myArray: string[] = ["apple", "banana", "orange"];

myArray[1].slice(3); // returns "ana"

```

## Deep Dive

The substring extraction methods in TypeScript are based on the ones in JavaScript, and they both have slightly different behaviors. In TypeScript, the substring() method will throw an error if the starting index is greater than the ending index, while in JavaScript, it will swap the two values and still return the substring. Additionally, the substr() method in TypeScript does not support negative starting indices, while it does in JavaScript.

It's also worth noting that these methods are not just limited to strings and arrays. They can also be used on objects or any data type that has a length property.

## See Also

Here are some helpful links for further reading on substring extraction in TypeScript:

- [TypeScript Documentation](https://www.typescriptlang.org/docs/)
- [MDN Web Docs for JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript)
- [W3Schools](https://www.w3schools.com/js/)