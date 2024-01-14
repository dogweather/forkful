---
title:    "Javascript recipe: Converting a string to lower case"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why

Converting a string to lower case may seem like a simple task, but it can actually be quite useful in various programming scenarios. By converting all the characters in a string to lower case, you can ensure that the input is standardized and consistent, making it easier to manipulate and compare with other strings.

## How To

To convert a string to lower case in Javascript, we can use the `toLowerCase()` function. This function takes in a string as an argument and returns a new string with all characters converted to lower case.

```Javascript
var myString = "Hello World!";
var lowerCaseString = myString.toLowerCase();

console.log(lowerCaseString);
// output: hello world!
```

As you can see, the `toLowerCase()` function simply returns a new string with all the characters in lower case. This can be useful when comparing strings that may have been entered in different cases, as it allows for easier validation and manipulation.

We can also use this function to convert user input to lower case. This ensures that the input is consistent and avoids potential errors or bugs caused by different case inputs.

```Javascript
var userInput = prompt("Enter your name: ");
var lowerCaseName = userInput.toLowerCase();

alert("Hello " + lowerCaseName + "!");
// output: Hello john!
```

## Deep Dive

It's important to note that the `toLowerCase()` function only works on strings in the ASCII character set. This means it will only convert characters that are within the range of a-z and A-Z. Any characters outside of this range, such as symbols or foreign characters, will not be affected by the function.

Another thing to keep in mind is that the `toLowerCase()` function does not modify the original string. It creates a new string with the converted characters, leaving the original string unchanged. This is important to consider when working with mutable data types in Javascript.

Additionally, the `toLowerCase()` function can also be useful when sorting or searching through arrays of strings. By converting all the strings to lower case, we can easily compare and sort them without worrying about case sensitivity.

## See Also

- [Javascript String Reference on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String)
- [String.prototype.toLowerCase() on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [ASCII Character Set](https://www.w3schools.com/charsets/ref_html_ascii.asp)