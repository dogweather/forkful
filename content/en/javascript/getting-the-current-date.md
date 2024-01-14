---
title:                "Javascript recipe: Getting the current date"
programming_language: "Javascript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why
In today's digital world, accurate and up-to-date information is crucial, especially when it comes to conducting operations or making decisions based on time-sensitive data. In the realm of programming, getting the current date is an essential task, and understanding how to do it can improve the functionality and reliability of any application or website.

## How To
Getting the current date in JavaScript is a simple yet powerful process that involves utilizing the built-in Date object. To get started, all you need to do is declare a new instance of the Date object and assign it to a variable, like this:

```javascript
let currentDate = new Date();
```

You can also specify a specific date and time by passing in the year, month, day, hour, minute, and second parameters to the Date constructor, like this:

```javascript
let specificDate = new Date(2021, 9, 15, 12, 30, 45);
```

To output the current date, you can use the `getDate()`, `getMonth()`, and `getFullYear()` methods, respectively, to retrieve the day, month, and year of the date stored in the Date instance. Here's an example:

```javascript
console.log(currentDate.getDate()); // Output: 15
console.log(currentDate.getMonth()); // Output: 9 (October)
console.log(currentDate.getFullYear()); // Output: 2021
```

In addition, there are various other methods to retrieve the current time, day of the week, and even milliseconds. Check out the "Deep Dive" section for more information.

## Deep Dive
The Date object in JavaScript is a powerful tool that allows for precise manipulation and retrieval of date and time information. Here are some additional methods that can be used to get various details from a Date instance:

- `getDay()` - Returns the day of the week (0 for Sunday, 1 for Monday, etc.)
- `getHours()` - Returns the hour value (from 0 to 23)
- `getMinutes()` - Returns the minute value (from 0 to 59)
- `getSeconds()` - Returns the second value (from 0 to 59)
- `getMilliseconds()` - Returns the millisecond value (from 0 to 999)

In addition to these methods, the Date object also has setters that allow for easy modification of the current date and time. For example, you can use the `setDate()` method to change the day of the month or the `setFullYear()` method to change the year. These setters are useful when working with date calculations or creating custom date formats.

## See Also
- [MDN Web Docs - Date object](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Date and time in JavaScript](https://javascript.info/date)
- [W3Schools - JavaScript Dates](https://www.w3schools.com/js/js_dates.asp)