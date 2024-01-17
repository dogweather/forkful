---
title:                "Extracting substrings"
html_title:           "Javascript recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?

Extracting substrings in Javascript refers to the process of retrieving a specific part of a string. This can be useful in various scenarios, such as manipulating user input or parsing API responses. By extracting substrings, programmers can effectively work with smaller chunks of text instead of having to manipulate the entire string.

## How to:

To extract a substring in Javascript, we can use the `substr()` method. This method takes two arguments, the starting index and the length of the substring we want to extract. For example, let's say we have the string "Hello World" and we want to extract the word "World" from it. We can use the following code:

```Javascript
let str = "Hello World";
let substr = str.substr(6, 5);
console.log(substr);

// Output: World
```

We can also use negative numbers as the starting index, which will count from the end of the string. For instance, to extract the last 3 characters of the string "Hello World", we can use the following code:

```Javascript
let str = "Hello World";
let substr = str.substr(-3);
console.log(substr);

// Output: rld
```

## Deep Dive:

The `substr()` method was first introduced in JavaScript 1.0, and it is still widely used today. However, there are alternatives that have been added in newer versions of JavaScript, such as the `substring()` and `slice()` methods. These methods also extract substrings, but they take different arguments and allow for more flexibility.

When using the `substr()` method, it's important to keep in mind that the second argument specifies the length of the substring, not the ending index. This can cause confusion and unexpected results, especially when dealing with negative numbers. It's also worth noting that the `substr()` method is not supported in Internet Explorer 8 or earlier versions.

## See Also:

- [MDN Web Docs - substr()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substr)
- [W3Schools - JavaScript String substr() Method](https://www.w3schools.com/jsref/jsref_substr.asp)
- [GeeksforGeeks - JavaScript | substr() function](https://www.geeksforgeeks.org/javascript-substr-function/)