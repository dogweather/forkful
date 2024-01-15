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

## Why
Finding the length of a string is a common task in web development. It allows developers to manipulate strings in various ways, such as validating input fields or formatting data for display. Plus, who doesn't love a good challenge?

## How To
```Javascript
// Method 1: .length property
const myString = "Hello World";
console.log(myString.length); // Output: 11

// Method 2: for loop
let count = 0;
for (let i = 0; i < myString.length; i++) {
  count++;
}
console.log(count); // Output: 11
```

Finding the length of a string can be achieved in a few different ways. The first method is by using the `.length` property, which is built-in to every string in JavaScript. This method is simple and straightforward, making it a popular choice. The second method involves using a for loop to iterate through each character of the string and counting the number of iterations. This method may be useful for more advanced string manipulation.

## Deep Dive
Behind the scenes, JavaScript strings are objects with their own properties and methods. This includes the `.length` property, which returns the number of characters in a string. This property is read-only, meaning it cannot be modified. It is also worth noting that the `.length` property not only counts alphabetical letters, but also other characters (like spaces and punctuation) and symbols (like emojis) within the string.

Additionally, it is important to keep in mind that the `.length` property counts the number of elements in a string, not the number of bytes or characters. This means that the length may be different for strings containing non-ASCII characters, as these may be represented by multiple bytes or characters.

## See Also
- [MDN Web Docs: String.length](https://developer.mozilla.org/en-US/docs/Web/Javascript/Reference/Global_Objects/String/length)
- [W3Schools: JavaScript Strings](https://www.w3schools.com/js/js_strings.asp)