---
title:    "Javascript recipe: Comparing two dates"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Why

As a programmer, there are many situations where you would need to compare two dates in your code. Whether you're working on a project that involves scheduling, online booking, or tracking data over time, being able to accurately compare dates is an important skill to have. In this blog post, we'll dive into the steps of how to compare dates in Javascript.

## How To

To begin, let's create two date objects using the `Date()` constructor:

```Javascript
let date1 = new Date('01/01/2021');
let date2 = new Date('05/10/2021');
```

To compare these two dates, we can use the `getTime()` method which returns the time value in milliseconds. Then, we can simply use the greater than (`>`) or less than (`<`) operators to compare the values. Here's an example:

```Javascript
if (date1.getTime() > date2.getTime()) {
  console.log('date1 is greater than date2');
} else {
  console.log('date2 is greater than date1');
}
```

The output of this code would be:

```
date2 is greater than date1
```

If you need to know if two dates are equal, you can use the `===` operator instead of `>` or `<`.

## Deep Dive

It's important to note that comparing dates in Javascript also takes into account time zones. This means that two dates can have the same day, month, and year but still be considered different if they are in different time zones. To avoid this issue, it's recommended to use the `getUTCDate()` and `getUTCMonth()` methods to get the UTC (Coordinated Universal Time) values of the dates and then compare them.

Additionally, you can also use the `getFullYear()`, `getMonth()`, and `getDate()` methods to get the individual year, month, and day values of a date. This can be useful if you want to compare specific parts of dates.

## See Also

- [Date Object in Javascript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Comparing Dates in Javascript](https://www.geeksforgeeks.org/how-to-compare-two-dates-in-javascript/)
- [Working with Dates and Times in Javascript](https://www.digitalocean.com/community/tutorials/understanding-date-and-time-in-javascript)