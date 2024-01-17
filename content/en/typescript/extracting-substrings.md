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

## What & Why?
Extracting substrings is the process of retrieving a part of a string based on a specified start and end position. Programmers often do this to manipulate or analyze data within strings, such as retrieving a username from an email address or extracting a specific date from a longer string.

## How to:
To extract a substring in TypeScript, you can use the built-in ```substring()``` method. The method takes in two arguments, the start index and the end index of the desired substring. For example:

```
let str: string = "Hello World";

console.log(str.substring(0, 5)); // Output: "Hello"
console.log(str.substring(6)); // Output: "World"
```

You can also use ```slice()``` method which works the same way as ```substring()```. The only difference is that ```slice()``` allows you to use negative index values, where -1 would refer to the last character, -2 would refer to the second to last character, and so on. Example:

```
let str: string = "Hello World";

console.log(str.slice(0, 5)); // Output: "Hello"
console.log(str.slice(6)); // Output: "World"
```

## Deep Dive:
Extracting substrings has been a commonly used method for manipulating strings since the early days of programming. However, with the introduction of regular expressions, developers now have an alternative method for extracting substrings. Regular expressions provide more powerful and flexible ways to not only extract substrings but also perform pattern matching and replacement within strings.

In TypeScript, you can use the ```match()``` method with a regular expression to extract substrings. Example:

```
let str: string = "The quick brown fox jumps over the lazy dog";

let result = str.match(/quick/);
console.log(result[0]); // Output: "quick"
```

It is worth noting that the ```match()``` method returns an array of matched strings, whereas ```substring()``` and ```slice()``` only return a single string.

## See Also:
- [MDN web docs - substring()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [MDN web docs - slice()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- [MDN web docs - regular expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)