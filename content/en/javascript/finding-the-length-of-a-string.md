---
title:                "Finding the length of a string"
html_title:           "Javascript recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Finding the length of a string means determining the number of characters or symbols present in a given string of text. This is a commonly used operation in programming, as it allows developers to manipulate and analyze strings in their code.

## How to:
```Javascript
// Using the `length` property of a string:
let myString = "Hello World!";
console.log(myString.length); // Output: 12 (11 letters + 1 space)

// Using the `string.length()` method:
let myString = "This is a sentence.";
console.log(myString.length()); // Output: 19 (18 characters + 1 space)
```

## Deep Dive:
There are a few alternative methods for finding the length of a string, such as using a `for` loop to iterate through the characters and counting them, or using the `substring()` method to split the string into an array and returning the length of the array. However, the `length` property and `length()` method are the most commonly used and efficient methods.

In earlier versions of JavaScript, the `string.length()` method did not exist and the `length` property was used to return the string's length as well as the length of other data types like arrays and objects. However, with the introduction of ECMAScript 2015, the `string.length()` method became available and is now the preferred method for finding string length.

## See Also:
- [MDN Web Docs on string.length()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [MDN Web Docs on String.length property](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [W3Schools: JavaScript String Length](https://www.w3schools.com/jsref/jsref_length_string.asp)
- [JavaScript.info: The length property and method](https://javascript.info/string#length-property-method)