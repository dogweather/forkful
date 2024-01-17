---
title:                "Comparing two dates"
html_title:           "TypeScript recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why?

Comparing two dates is the process of determining the relationship between two given dates. This comparison is commonly done by programmers when working with date data to check for things like time intervals, date differences, or comparing for equality.

## How to:

To compare two dates in TypeScript, we can use the built-in `Date` object. Below are some examples using different methods to compare dates:

```TypeScript
// Compare for equality
const date1 = new Date('2021-01-01');
const date2 = new Date('2021-01-01');

console.log(date1 === date2); // false (since it is comparing objects, not values)
console.log(date1.getTime() === date2.getTime()); // true (compares the millisecond values)

// Check if date1 is before date2
const date3 = new Date('2021-01-01');
const date4 = new Date('2021-01-15');

console.log(date3 < date4); // true

// Check if date1 is after date2
const date5 = new Date('2021-01-01');
const date6 = new Date('2021-01-15');

console.log(date5 > date6); // false
```

## Deep Dive:

The concept of comparing dates has been around for a long time, even before computers existed. In the past, it was essential for people to compare dates to keep track of time and schedule events. With the advancement of technology, this task has been passed on to programmers who can now easily compare dates using programming languages.

While the `Date` object is the most common way to compare dates in TypeScript, there are also other options available. For example, third-party libraries like Moment.js provide more functionalities and options for comparing dates. Additionally, some programming languages have built-in methods for comparing dates, but TypeScript does not currently have this capability.

To compare dates accurately, it is important to understand time zones and daylight saving time. When comparing dates from different time zones, it is essential to convert them to the same time zone before comparison to avoid any discrepancies.

## See Also:

- [Date.prototype.getTime() - MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/getTime)
- [Moment.js - Official Website](https://momentjs.com/)
- [Date and time in programming - Wikipedia](https://en.wikipedia.org/wiki/Date_and_time_representations_by_country)