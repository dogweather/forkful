---
title:                "Converting a date into a string"
html_title:           "TypeScript recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Converting a date into a string in TypeScript means taking a date object and transforming it into a string representation. Programmers often do this to display dates in a specific format or to easily manipulate and compare dates.

## How to:

```TypeScript
const date = new Date();

// Convert a date into a string using toDateString() method
const dateString = date.toDateString();
console.log(dateString); // "Thu Sep 09 2021"

// Convert a date into a custom string format using toLocaleDateString() method
const options = { weekday: 'long', year: 'numeric', month: 'long', day: 'numeric' }; // specify format options
const customString = date.toLocaleDateString('en-US', options); // get string in specified format
console.log(customString); // "Thursday, September 9, 2021"
```

## Deep Dive:

Converting a date into a string has been a common task for programmers since the early days of computing. In the past, this was often done by using a combination of different functions to manipulate and display the different components of a date (day, month, year, etc.). The introduction of specific methods like `toDateString()` and `toLocaleDateString()` in languages like TypeScript has made this process much simpler and more efficient.

There are also alternatives to converting a date into a string, such as using third-party libraries like Moment.js or using JavaScript's built-in `toLocaleString()` method. However, the built-in methods in TypeScript offer more flexibility and customization options.

When converting a date into a string, it's important to consider the format and locale. Different regions and languages have varying conventions for displaying dates. The `toLocaleDateString()` method allows you to specify the desired format and locale, making it easier to display dates in a way that is familiar and easily understandable to your audience.

## See Also:

- [MDN web docs on Date object](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js](https://momentjs.com/)
- [JavaScript toLocaleString() method](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleString)