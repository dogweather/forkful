---
title:                "Javascript recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why

Capitalizing a string may seem like a simple task, but it can be incredibly useful in improving the readability of our code and presenting data in a more organized manner. Additionally, some programming languages require specific formatting for strings, making it necessary to know how to capitalize a string properly.

## How To

In Javascript, there are different methods we can use to capitalize a string, each with its own advantages and use cases. Let's take a look at some coding examples using the ```toUpperCase()``` and ```charAt()``` methods:

```Javascript

// Using the toUpperCase() method

let name = "john smith";
let capitalized = name.toUpperCase();

console.log(capitalized); // Output: JOHN SMITH

// Using the charAt() method

let name = "jane doe";
let firstLetter = name.charAt(0).toUpperCase() + name.slice(1);

console.log(firstLetter); // Output: Jane doe
```

In the first example, we use the ```toUpperCase()``` method to convert all letters in the string to uppercase. This method is simple and effective, but it has a limitation - it makes all letters in the string uppercase, which may not be ideal in certain situations.

To overcome this limitation, we can use the ```charAt()``` method to target specific letters and capitalize only those letters. In the second example, we use the ```charAt(0)``` to select the first letter of the string and then use ```toUpperCase()``` to capitalize it. We then use ```slice(1)``` to select the rest of the string and concatenate it with the capitalized letter.

There are other methods and techniques for capitalizing strings in Javascript, each with its own nuances and applications. It is important to understand these methods and choose the one that best suits our specific needs.

## Deep Dive

The ```toUpperCase()``` method is a built-in function in Javascript that converts a string to uppercase letters. This method does not modify the original string, but instead returns a new string with all uppercase letters.

The ```charAt()``` method returns the character at a specified index in a string. In our example, we used ```charAt(0)``` to select the first letter of the string. We can also use it to target other letters, such as ```charAt(1)``` for the second letter or ```charAt(4)``` for the fifth letter.

It is important to note that the ```charAt()``` method returns an empty string if the specified index is out of range. So, if we try to select a letter that is not present in the string, like ```charAt(10)```, it will return an empty string.

In addition to these methods, we can also use regular expressions to capitalize strings in Javascript. Regular expressions are powerful tools for pattern matching and manipulating strings, but they require a deeper understanding of the language.

## See Also

- [MDN Web Docs - String.prototype.toUpperCase method](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [MDN Web Docs - String.prototype.charAt method](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/charAt)
- [MDN Web Docs - RegExp](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RegExp)
- [W3Schools - JavaScript Tutorial](https://www.w3schools.com/js/default.asp)