---
title:                "Parsing a date from a string"
html_title:           "Javascript recipe: Parsing a date from a string"
simple_title:         "Parsing a date from a string"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Parsing a date from a string in Javascript refers to converting a date written in a string format into a date object that can be recognized and manipulated by the computer. Programmers frequently do this to store and manipulate dates in their code, such as creating a calendar or setting an expiration date for a product.

## How to:
To parse a date from a string in Javascript, use the built-in Date.parse() method. This method takes in a string representing a date and returns the number of milliseconds since January 1, 1970. 

Example:
```Javascript
const dateString = 'January 12, 2021';
const date = Date.parse(dateString);
console.log(date); // Output: 1610448000000
```

To convert this number of milliseconds into a readable date, use the Date() constructor.

Example:
```Javascript
const date = new Date(1610448000000);
console.log(date.toDateString()); // Output: Tue Jan 12 2021
```

## Deep Dive:
Parsing dates from strings has been a common problem for programmers since the early days of computing. Prior to the creation of built-in methods, developers had to create their own algorithms to parse dates. However, with the advent of the Date.parse() method in Javascript, this task has become much simpler.

An alternative to the Date.parse() method is using a library like Moment.js. This library provides a more comprehensive and user-friendly way to parse and manipulate dates in Javascript. 

Implementation details of the Date.parse() method differ between browsers and can sometimes lead to unexpected results. It's important to test your code thoroughly across different browsers and versions to ensure consistent behavior.

## See Also:
- [MDN Web Docs: Date.parse()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/parse)
- [Moment.js](https://momentjs.com/)