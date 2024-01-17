---
title:                "Getting the current date"
html_title:           "TypeScript recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?

Getting the current date means finding out the current date and time on your computer or server. Programmers do this to keep track of when certain operations were performed, to schedule tasks, or to display the current time on a user interface.

## How to:

To get the current date in TypeScript, you can use the built-in `Date` class. Here's an example of getting the current date and time and storing it in a variable:

```TypeScript
let currentDate = new Date();
console.log(currentDate);
```

This will output the date and time in a format like `Sat Nov 28 2020 14:37:48 GMT-0500 (Eastern Standard Time)`.

If you want to customize the format of the date, you can use the `toLocaleString()` method. Here's an example of getting the current date in a specific format and storing it in a variable:

```TypeScript
let currentDate = new Date().toLocaleString("en-US", { weekday: 'long', year: 'numeric', month: 'long', day: 'numeric' });
console.log(currentDate);
```

This will output the date in a format like `Saturday, November 28, 2020`.

## Deep Dive:

The `Date` class in TypeScript is based on the `Date` object in JavaScript, which has been around since the 90s. This class provides methods for working with dates and times, such as getting the current date, setting a specific date, or calculating the difference between two dates.

An alternative to the `Date` class is the `Moment.js` library, which provides more flexibility and functionality for working with dates and times. However, it requires an additional installation and may be overkill for simple date operations.

To get the current date, the `Date` class uses the system's time zone and clock. This means that if the system time or time zone is incorrect, the current date returned by the `Date` class will also be incorrect.

## See Also:

- [TypeScript Date Documentation](https://www.typescriptlang.org/docs/handbook/standard-library.html#date)
- [Moment.js Documentation](https://momentjs.com/docs/)
- [MDN Web Docs - Date Object](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)