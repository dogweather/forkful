---
title:                "Finding the length of a string"
html_title:           "Arduino recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Finding the length of a string in JavaScript means determining the number of characters in that string. Programmers often need this data to manage text input restrictions, data validation, or loop control.

## How to:
JavaScript offers an easy way to find the length of a string using the `length` property. Here's how you do it:

```Javascript
let str = "Hello, World!";
console.log(str.length); // outputs: 13
```

And, voila! You've got the number of characters in your string.

## Deep Dive

### Historical Context
JavaScript was first released in 1995, and the `length` property has been there since the beginning. It goes back to the language's core focus on textual data processing for the web.

### Alternatives
While `length` is the out-of-the-box, most efficient, and straightforward way to get string length, you can also count characters with an iterative loop if you're feeling adventurous.

```Javascript
let str = "Hello, World!";
let count = 0;
for(let i=0; i<str.length; i++) {
    count++;
}
console.log(count); // outputs: 13
```
This is less efficient but shows manual character counting.

### Implementation Details
The `length` property of a string in JavaScript counts the UTF-16 units in a string, not the number of characters. This distinction matters when dealing with non-Basic-Multilingual-Plane symbols, where a single character could be represented as two 16-bit units.

## See Also
MDN's [length property documentation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length) dives deeper into what's happening underneath the hood when you access `string.length`. Also, give [Iterating over strings](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Indexed_collections#strings) a read. It presents looping over a string as an array of characters, which is an interesting application of string length in practice.