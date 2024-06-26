---
date: 2024-01-20 17:45:59.182526-07:00
description: ''
lastmod: '2024-04-05T21:59:47.727254-06:00'
model: gpt-4-1106-preview
summary: ''
title: Extracting substrings
weight: 6
---

## How to:


### Using `substring` method:
```javascript
let text = "JavaScript is awesome!";
let extracted = text.substring(0, 10);
console.log(extracted); // Output: JavaScript
```

### Using `slice` method:
```javascript
let text = "JavaScript is awesome!";
let sliced = text.slice(-9, -1);
console.log(sliced); // Output: awesome
```

### Using `substr` method (deprecated):
```javascript
let text = "JavaScript is awesome!";
let substrd = text.substr(11, 7);
console.log(substrd); // Output: awesome
```

## Deep Dive
Extracting substrings isn't new – it's as old as programming itself. The `substring` and `slice` methods in JavaScript are tools from the 1990s, part of the language's initial feature set. `substr` was also in there, but it's now legacy code and should be avoided in modern applications. 

The difference? `substring` and `slice` are similar – both take start and end index parameters – but handle negatives differently: `slice` can handle negative indices, counting from the end, while `substring` treats them as zeroes. All these methods don’t mutate the original string; they produce new ones.

## See Also
- Mozilla Developer Network on Strings: [MDN Web Docs - String](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String)
- String manipulation with JavaScript: [W3Schools - JavaScript String Methods](https://www.w3schools.com/js/js_string_methods.asp)
- JavaScript string basics: [JavaScript.info - Strings](https://javascript.info/string)
