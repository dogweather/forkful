---
title:                "Javascript recipe: Converting a string to lower case"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why

When working with strings in Javascript, it is common to encounter situations where you need to convert a string to lower case. This is useful for various reasons such as string comparison or when displaying user input in a consistent format.

## How To

To convert a string to lower case in Javascript, we can use the `toLowerCase()` function. This function is available on any string object and it will return a new string with all the characters converted to lower case.

```Javascript
const name = "JOHN DOE";
const lowercaseName = name.toLowerCase();

console.log(lowercaseName);
// Output: "john doe"
```

We can also use this function on user input to ensure that it is displayed in a consistent format. For example:

```Javascript
const userInput = prompt("Enter your name:");

// Input: "JaSon Li"

console.log(userInput.toLowerCase());
// Output: "jason li"
```

Another way to convert a string to lower case is by using the `String.toLowerCase()` method. This method can be used directly on a string without first creating a string object.

```Javascript
const name = "MIA SMITH";
const lowercaseName = String.toLowerCase(name);

console.log(lowercaseName);
// Output: "mia smith"
```

## Deep Dive

It is important to note that when using the `toLowerCase()` function, the original string remains unchanged. As strings are immutable in Javascript, any string manipulation will always create a new string. This means that the `toLowerCase()` function does not modify the original string, but instead returns a new string with the lower case characters.

It is also worth mentioning that the `toLowerCase()` function only converts characters to lower case that are defined by Unicode. This means that any non-alphabetical characters will remain unchanged.

## See Also

- [JavaScript String toLowerCase() Function](https://www.w3schools.com/jsref/jsref_tolowercase.asp)
- [MDN Web Docs: toLowerCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)