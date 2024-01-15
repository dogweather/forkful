---
title:                "Getting the current date"
html_title:           "Javascript recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why

Wondering what day it is today? Want to assign the current date to a variable in your code? Whatever the reason may be, getting the current date is a useful skill to have in your JavaScript arsenal.

## How To

To get the current date in JavaScript, we can use the `Date()` object. This object represents a specific moment in time and has methods that allow us to retrieve various pieces of information about the current date and time.

First, let's create a new instance of the `Date()` object and assign it to a variable:

```Javascript
let currentDate = new Date();
```

This will store the current date and time in the `currentDate` variable. Now, we can use various methods to extract specific information from this object.

For example, we can use the `getDate()` method to retrieve the day of the month:

```Javascript
let day = currentDate.getDate();
console.log(day);
// Output: 25 (if today is the 25th)
```

We can also use the `getMonth()` method to retrieve the current month, but be aware that the month is zero-indexed, meaning January is represented by 0 and December by 11. So, we will often add 1 to the result to get the actual month number:

```Javascript
let month = currentDate.getMonth() + 1;
console.log(month);
// Output: 7 (if today is in July)
```

If we want to get the full year, we can use the `getFullYear()` method:

```Javascript
let year = currentDate.getFullYear();
console.log(year);
// Output: 2021 (if we are in the year 2021)
```

We can also get the current time using the `getHours()`, `getMinutes()`, and `getSeconds()` methods:

```Javascript
let hours = currentDate.getHours();
let minutes = currentDate.getMinutes();
let seconds = currentDate.getSeconds();
console.log(hours + ":" + minutes + ":" + seconds);
// Output: 16:42:30 (if the current time is 16:42:30)
```

There are many other methods available on the `Date()` object to retrieve different pieces of information. Experiment with them and see what you can do!

## Deep Dive

As mentioned earlier, the `Date()` object represents a specific moment in time. This means that it is not just limited to the current date and time. We can also use it to represent a specific date and time in the past or future.

To do this, we can pass in a specific date and time as a parameter when creating a new instance of the `Date()` object. For example, if we want to represent July 4th, 1776 at 5:00 pm, we can do the following:

```Javascript
let freedomDay = new Date(1776, 6, 4, 17, 0, 0);
console.log(freedomDay);
// Output: Thu Jul 04 1776 17:00:00 GMT-0700 (Pacific Daylight Time)
```

Notice how we passed in the month, day, hour, minute, and second as separate parameters. This allows us to create a specific moment in time rather than just the current date and time.

It's also worth mentioning that the `Date()` object can handle time zones, leap years, and daylight saving time. It's a powerful tool for working with dates and times in JavaScript.

## See Also

- [MDN web docs - Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [W3Schools - JavaScript Date](https://www.w3schools.com/js/js_dates.asp)
- [Stack Overflow - How to get current date/time in JavaScript](https://stackoverflow.com/questions/42861864/how-to-get-current-date-time-in-javascript)