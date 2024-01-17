---
title:                "Capitalizing a string"
html_title:           "Javascript recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Capitalizing a string is the process of converting the first letter of each word in a string to uppercase while keeping the rest of the letters lowercase. This is commonly done in programming to improve the readability of data and make it easier to distinguish between different words and variables.

## How to:
To capitalize a string in JavaScript, there are a few different approaches you can use depending on your specific use case.

### Basic Method:
One way to capitalize a string is to use the `toUpperCase()` function, which converts all letters in a string to uppercase. To only capitalize the first letter, you can use the `slice()` function to extract the first letter and add it to the result of `toUpperCase()`.

```javascript
let str = 'hello world';
let capitalizedStr = str[0].toUpperCase() + str.slice(1).toLowerCase();

console.log(capitalizedStr); // Output: Hello world
```

### Regex Method:
Another way to capitalize a string is by using a regular expression to match the first letter of each word and replace it with its uppercase version. This approach may be more useful when dealing with longer or more complex strings.

```javascript
let str = 'javascript is awesome';
let capitalizedStr = str.replace(/\b\w/g, (char) => char.toUpperCase());

console.log(capitalizedStr); // Output: Javascript Is Awesome
```

## Deep Dive:
The practice of capitalizing strings can be traced back to traditional writing conventions, where the first letter of a sentence or title is typically capitalized. In programming, this convention is used to make data more human-readable and improve the overall code quality.

There are also other variations of capitalizing strings, such as capitalizing all letters or only the first letter of the first word. These methods may be used in different programming languages or depending on the specific purpose of the string.

## See Also:
- MDN Web Docs: [toUpperCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- Stack Overflow: [How do I make a string's first letter uppercase in JavaScript?](https://stackoverflow.com/questions/1026069/how-do-i-make-the-first-letter-of-a-string-uppercase-in-javascript)