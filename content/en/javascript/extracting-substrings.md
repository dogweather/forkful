---
title:                "Javascript recipe: Extracting substrings"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why

As a programmer, one of the most common tasks we face is manipulating strings. And sometimes, we may need to extract a specific part of a string for further processing. This is where the concept of extracting substrings comes in. It allows us to extract a portion of a string based on certain conditions and use it in our code.

## How To

To extract substrings in Javascript, we can use the built-in substring() method. It takes two parameters: the starting index and the ending index of the substring. For example:

```Javascript
let str = "Hello World";
let substr = str.substring(6, 11);
console.log(substr); // Output: "World"
```

In this example, we start extracting from index 6 (which corresponds to the letter "W" in "World") and end at index 11. It's important to note that the ending index is not included in the substring.

We can also omit the second parameter and the substring will be extracted from the starting index till the end of the string. For instance:

```Javascript
let substr = str.substring(3); // Output: "lo World"
```

Additionally, we can use negative numbers to count from the end of the string. For example, using -1 as the starting index will extract the last character of the string.

```Javascript
let str = "Hello World";
let lastChar = str.substring(-1); // Output: "d"
```

## Deep Dive

Under the hood, the substring() method creates a new substring by copying characters from the original string based on the given parameters. It's worth mentioning that this method does not change the original string, but returns a new one instead.

Another important thing to note is that the substring() method only works with positive indices. If we try to use a negative index as the starting position, it will be treated as 0.

Moreover, the substring() method is different from the slice() method in that it cannot handle negative indices or swap the starting and ending positions. It also does not accept regular expressions as parameters.

## See Also

- [MDN Web Docs on substring()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [W3Schools Tutorial on Substrings in JavaScript](https://www.w3schools.com/jsref/jsref_substring.asp)
- [DevDocs Documentation on String Manipulation in JavaScript](https://devdocs.io/javascript/global_objects/string)