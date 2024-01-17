---
title:                "Das aktuelle Datum erhalten"
html_title:           "Javascript: Das aktuelle Datum erhalten"
simple_title:         "Das aktuelle Datum erhalten"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Was & Warum?
Getting the current date (das aktuelle Datum) is a fundamental task in programming, as it allows us to display the current date and time in our programs. This is especially important for applications that rely on time-sensitive functions, such as calendars, scheduling, and event-based systems.

## So geht's:
```
// Code block 1: Using the built-in Date() constructor to get the current date
let currentDate = new Date();
console.log(currentDate);
// Output: Wed Sep 08 2021 19:57:04 GMT+0200 (Central European Summer Time)

// Code block 2: Getting the current date in a specific format
let currentDate = new Date();
let day = currentDate.getDate();
let month = currentDate.getMonth() + 1; // January is represented by 0, so we need to add 1 to get the correct month
let year = currentDate.getFullYear();
console.log(`${day}/${month}/${year}`);
// Output: 08/09/2021 
```

## Tief tauchen:
The concept of getting the current date can be traced back to the first programmable computers in the 1940s. However, the Date() object was introduced in JavaScript in 1995, making it easier for developers to handle dates and times in their programs. Alternative ways of getting the current date include using a third-party library like moment.js or using the new Date.now() method introduced in ES5.

## Siehe auch:
- MDN Web Docs: [Date()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- W3Schools: [JavaScript Date()](https://www.w3schools.com/jsref/jsref_obj_date.asp)
- Moment.js: [Parsing, validating, manipulating, and formatting dates](https://momentjs.com/)