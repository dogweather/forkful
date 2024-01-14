---
title:    "TypeScript recipe: Calculating a date in the future or past"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why

In today's fast-paced world, it is important to be able to calculate dates accurately and efficiently. Whether it is for scheduling appointments, planning events, or managing deadlines, being able to calculate a date in the future or past is a useful skill for any programmer. With the help of TypeScript, this task can easily be achieved.

## How To

To calculate a date in the future or past using TypeScript, there are a few steps that can be followed:

1. First, we will need to create a new date object using the `Date()` constructor. This will give us the current date and time.
```
TypeScript
let currentDate: Date = new Date();
```
2. Next, we will need to specify the number of milliseconds to add or subtract from the current date and time to get the desired future or past date. This can be done by using the `getTime()` method, which returns the number of milliseconds since January 1, 1970, and then multiplying it by the desired time interval (e.g. 24 hours, 7 days, etc.).
```
TypeScript
let futureDate: Date = new Date(currentDate.getTime() + (24 * 60 * 60 * 1000));
// This will give us the future date and time by adding 24 hours to the current date and time.
```
3. Finally, to get a formatted output, we can use the `toLocaleDateString()` or `toLocaleTimeString()` methods, depending on our preference.
```
TypeScript
console.log(futureDate.toLocaleDateString());
// Output: 7/7/2021
console.log(futureDate.toLocaleTimeString());
// Output: 12:00:00 AM
```

## Deep Dive

The calculation of dates in the future or past can be more complex depending on various factors like leap years, time zones, and daylight savings. One important thing to note is that JavaScript/TypeScript's `Date` object deals only with local time. This means that if you are working with different time zones, you will need to take that into consideration when calculating a future or past date.

Additionally, there are many useful libraries and packages available for handling dates and times in a more robust and intuitive way in TypeScript. These include libraries such as Moment.js, Day.js, and Date-fns, which provide more advanced functionalities for date calculations.

## See Also

- [TypeScript Date() Object Documentation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js Library](https://momentjs.com/)
- [Day.js Library](https://day.js.org/)
- [Date-fns Library](https://date-fns.org/)