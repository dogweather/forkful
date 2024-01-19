---
title:                "Parsing a date from a string"
html_title:           "C recipe: Parsing a date from a string"
simple_title:         "Parsing a date from a string"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Parsing a Date from a String in JavaScript

## What & Why?
Parsing a date from a string is extracting or converting a date stored in string format into a date object that JavaScript can recognize and manipulate. Programmers commonly do this to handle date and time data retrieved in string format from various sources, be it APIs, text files, user input or databases.

## How to:
Let's take a look at how you can parse a date from a string using JavaScript's built-in Date object. Suppose you have a string "2020-12-31".

```Javascript
let dateString = "2020-12-31";
let dateObject = new Date(dateString);
console.log(dateObject);
```

When you run this code, the output will be:

```Javascript
Date Thu Dec 31 2020 00:00:00 GMT+0000 (Coordinated Universal Time)
```
JavaScript automatically recognizes the date string in "YYYY-MM-DD" format and converts it to a Date object.

## Deep Dive
In the early days, there wasn't a standard way of parsing a date string in JavaScript. Different browsers interpreted dates in different ways. This discrepancy made handling dates a hectic task for developers. Thus, JavaScript introduces the standard ISO-8601 date format, which most date functions respect and understand nowadays for consistency.

You can also use libraries like Moment.js for more formatting options and better browser compatibility. Here's how you use Moment.js to parse a date.

```Javascript
let moment = require('moment');
let dateString = "31-12-2020";
let dateObject = moment(dateString, "DD-MM-YYYY");
console.log(dateObject.format());
```
In JavaScript, the Date object is built based on the time value that represents the number of milliseconds since the Unix Epoch (January 1, 1970, 00:00:00 UTC). When a date string is parsed, it is converted to this time value. This explains why date strings without time components default to midnight (00:00:00).

Keep in mind that the behavior of date parsing in JavaScript is slightly different when dealing with two-digit years. For example, new Date('01-01-70') considers 70 as 1970, not 2070.

## See Also
Visit the following resources to understand more about JavaScript date parsing.
- [MDN Web Docs - Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js Documentation](https://momentjs.com/docs/)
- [ECMAScript® 2018 Language Specification - Date Objects](https://www.ecma-international.org/ecma-262/9.0/index.html#sec-date-objects)