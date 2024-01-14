---
title:                "Javascript recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why

Converting a string to lower case can be a useful task when working with strings in JavaScript. It allows for consistency in comparison and can be used for displaying text in a more readable format. 

## How To

Converting a string to lower case in JavaScript is a straightforward process. Simply use the `toLowerCase()` method on the string variable. Let's take a look at an example:

```javascript
let name = "JOHN DOE";
console.log(name.toLowerCase());
```

The output of this code would be `john doe` in all lowercase letters.

We can also use the `toLowerCase()` to convert user input to lower case to ensure consistency in our code. Here's an example:

```javascript
let userInput = prompt("Enter your name: ");
console.log("Hello, " + userInput.toLowerCase() + "!");
```

If the user enters "SaRa", the output would be `Hello, sara!` 

## Deep Dive

Under the hood, the `toLowerCase()` method uses the Unicode character mapping and does not modify the original string variable. It only returns a new string with all the characters converted to lower case.

It's important to note that the `toLowerCase()` method only works on characters that have a lowercase equivalent. If a character does not have a lowercase equivalent, it will remain unchanged. For example, the Turkish alphabet has an additional lowercase character "ı" which does not have an upper case equivalent. This means that the string "Istanbul" would be converted to "ıstanbul" instead of "istanbul". 

We can also use the `toLowerCase()` method to convert individual characters in a string, not just the entire string. This can be useful when manipulating and formatting strings.

## See Also

For more information on string manipulation in JavaScript, check out these resources:

- [String.prototype.toLowerCase() - MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [Unicode character mapping - Unicode Consortium](https://home.unicode.org/)