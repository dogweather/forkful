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

## Why

In today's fast-paced world, having an accurate and up-to-date display of the current date is crucial for efficient time management and organization. Whether it's for scheduling events, tracking deadlines, or simply staying on top of daily tasks, knowing the current date is a fundamental need for many people.

## How To

To get the current date in TypeScript, you can use the built-in `Date` object. This object represents a single moment in time and provides methods for retrieving various date and time components.

```TypeScript
// Create a new Date object
let currentDate = new Date();

// Get the current day, month, year, and time
let day = currentDate.getDate();
let month = currentDate.getMonth() + 1; // 'getMonth()' returns a zero-based value
let year = currentDate.getFullYear();
let time = currentDate.toLocaleTimeString();

// Print out the current date and time in a user-friendly format
console.log(`Today is ${month}/${day}/${year} and the time is ${time}.`);
```

Running the code above will produce an output similar to this:

```
Today is 7/15/2021 and the time is 9:34:00 AM.
```

There are also options for customizing the format of the date and time, such as including the day of the week or using different time zones. You can explore the full list of `Date` object methods in the [TypeScript documentation](https://www.typescriptlang.org/docs/handbook/utility-types.html#partial).

## Deep Dive

For a deeper understanding of how the `Date` object works, it's important to note that it uses the UTC (Coordinated Universal Time) standard to represent dates and times. This means that the date and time will be based on the international time standard and not the local time zone of the user's device.

To account for this, the `toLocaleString()` method can be used to correctly display the date and time in the user's local time zone. Additionally, the `setDate()` and `setTime()` methods can be used to adjust the date and time values of the `Date` object to a specific date and time.

## See Also

- [TypeScript Documentation on Date Object](https://www.typescriptlang.org/docs/handbook/utility-types.html#partial)
- [GeeksforGeeks tutorial on getting current date in TypeScript](https://www.geeksforgeeks.org/how-to-get-the-current-date-and-time-in-typescript/)
- [MDN Web Docs article on working with dates and times in JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)