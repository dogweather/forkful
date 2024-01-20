---
title:                "Concatenating strings"
html_title:           "PHP recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?

Concatenating strings refers to linking multiple strings into just one new string. Programmers do this to build a custom message dynamically, often to output messages or send captive data.

## How To:

There are some ways to concatenate strings in JavaScript. Here they are:

1. Using the "+" operator:
```Javascript
let str1 = "Hello";
let str2 = "World!";
let message = str1 + " " + str2; 
console.log(message); // Displays "Hello World!"
```

2. Using the concat() function:
```Javascript
let str1 = "Hello";
let str2 = "World!";
let message = str1.concat(" ", str2);
console.log(message); // Displays "Hello World!"
```

3. Using ES6 template literals:
```Javascript
let str1 = "Hello";
let str2 = "World!";
let message = `${str1} ${str2}`; 
console.log(message); // Displays "Hello World!"
```

## Deep Dive:

The concept of concatenating strings has been around since early programming languages. Nowadays, JavaScript offers more efficient ways to carry out string concatenation, such as template literals introduced in ES6 to make string concatenation more readable.

While the "+" operator and the concat() function are straightforward and widely-used methods, they might become cumbersome when dealing with lengthy text and lots of strings. Template literals ease this situation significantly.

However, note that browser compatibility is a point of concern here. Some older browsers may not support template literals, so make sure your target audience’s browser supports it before using.

## See Also:

To deepen your understanding, make sure to check out these resources:

- MDN JavaScript String Documentation: [MDN Docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String)
- Understanding ES6 Template Literals: [ES6 Template Literals](https://wesbos.com/javascript-template-strings/)
- Browser Compatibility for Template Literals: [Can I Use](https://caniuse.com/#feat=template-literals)