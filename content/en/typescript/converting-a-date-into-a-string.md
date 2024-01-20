---
title:                "Converting a date into a string"
html_title:           "Arduino recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Converting a date into a string in TypeScript is just what it sounds like: transforming a Date object into a readable string format. This is handy when you want to display dates for users in a customized style or store the date data in a more readable way.

## How To:

In TypeScript, we can lean on the built-in `toDateString()` and `toISOString()` methods of the Date object. Let's see them in action.

```TypeScript
let today = new Date();

// Using toDateString
let dateString = today.toDateString();
console.log(dateString); // Output will be "Wed Sep 22 2021"

// Using toISOString
let isoString = today.toISOString();
console.log(isoString); // Output will resemble "2021-09-22T19:39:34.937Z"
```
The `toDateString()` method provides a more human-readable format, while `toISOString()` gives you a precise, standardized string that's ISO 8601 compliant.

## Deep Dive:

The Date object has been in JavaScript (and consequently in TypeScript) since its first edition. Before these convenient methods like `toISOString()`, developers had to manually slice and dice strings to manipulate date formats.

There are also several libraries, including day.js, moment.js, date-fns, that offer even more features for date and time manipulation. They are great for handling more complex tasks, but for simple conversions, the built-in methods are quite sufficient and efficient.

The actual implementation of `toDateString()` and `toISOString()` can vary among engines. In general, they take the date held in the Date object, apply appropriate localization or ISO standard rules, and construct a new string with the converted format.

## See Also:

- [Mozilla Developer Guide on Date Object](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [ISO 8601 Date and Time Format](https://www.iso.org/iso-8601-date-and-time-format.html)
- [Date libraries comparison: Moment.js vs date-fn vs Day.js](https://stackshare.io/stackups/day-js-vs-date-fns-vs-moment-js)