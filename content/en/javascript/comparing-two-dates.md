---
title:                "Javascript recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why

Comparing two dates may seem like a mundane task, but it can actually be quite useful in many programming scenarios. Whether you need to sort events in chronological order or check for overlapping time intervals, being able to compare dates is an essential skill for any Javascript programmer.

## How To

Comparing dates in Javascript can be done using the `Date` object and built-in methods. Let's take a look at some code examples to demonstrate how it works.

```
// Create two date objects
var date1 = new Date("2021-01-01");
var date2 = new Date("2021-02-01");

// Compare if date1 is before date2
if (date1 < date2) {
    console.log("date1 is before date2");
}
// Output: date1 is before date2

// Compare if date1 is equal to date2
if (date1.getTime() === date2.getTime()) {
    console.log("date1 is equal to date2");
}
// Output: None

// Compare if date1 is after date2
if (date1 > date2) {
    console.log("date1 is after date2");
}
// Output: None
```

In the first example, we use the `<` operator to check if `date1` is before `date2`. This works because the comparison is done based on the numeric value of the dates, which is the number of milliseconds that have passed since January 1, 1970.

In the second example, we use the `getTime()` method to get the numeric value of the dates, and then use strict equality `===` to check if they are equal. Keep in mind that even if two dates represent the same moment, they will still have different objects and therefore not be strictly equal.

The same logic can be applied to other comparison operators like `<=`, `>=`, `!=`, etc.

## Deep Dive

When comparing dates in Javascript, it's important to understand how the dates are being compared. As mentioned earlier, the comparison is based on the number of milliseconds elapsed since January 1, 1970. This is commonly known as the Unix Epoch time. This means that the comparison will be affected by timezones and daylight saving time. For example, if you compare two dates from different timezones, you may get unexpected results.

Another thing to keep in mind is that the `Date` object in Javascript has limited precision, with only milliseconds being the smallest unit. This means that comparing dates that are only a few milliseconds apart could give inaccurate results.

Lastly, when using the `getTime()` method to get the numeric value of a date, it's important to remember that it returns an integer. This means that any time information (hours, minutes, seconds) will be truncated.

To avoid these potential issues, it's recommended to use external libraries like Moment.js or Date-fns for more robust and precise date comparisons.

## See Also

- [MDN Web Docs - Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js](https://momentjs.com/)
- [Date-fns](https://date-fns.org/)