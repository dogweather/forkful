---
title:                "Comparing two dates"
html_title:           "Javascript recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why?
Comparing two dates is a way to determine whether one date is before, after, or equal to another date. Programmers often need to compare dates to sort data, calculate time intervals, or validate inputs. It is a fundamental operation in date and time processing, making it an essential skill in programming.

## How to:
To compare two dates in JavaScript, you can use the `Date` object, which represents a specific moment in time. Here's an example comparing two dates with the `>` operator:

```Javascript
const date1 = new Date('January 1, 2020');
const date2 = new Date('January 1, 2021');

if (date1 > date2) {
  console.log("date1 is after date2");
} else {
  console.log("date1 is before date2");
}

// Output: date1 is before date2
```

You can also use the `getTime()` method to get the time in milliseconds since January 1, 1970, and then compare these values. Here's an example:

```Javascript
const date1 = new Date('January 1, 2020').getTime();
const date2 = new Date('January 1, 2021').getTime();

if (date1 > date2) {
  console.log("date1 is after date2");
} else {
  console.log("date1 is before date2");
}

// Output: date1 is before date2
```

## Deep Dive
Date and time processing have come a long way since the early days of programming. In the past, programmers had to manually calculate time intervals and validate inputs, which was a time-consuming and error-prone process. Fortunately, with the introduction of the `Date` object and its various methods, date and time handling in JavaScript has become much easier.

One alternative to using the `Date` object for date comparison is using third-party libraries, such as Moment.js, js-Joda, or Day.js. These libraries offer more extensive features and better performance compared to the built-in `Date` object. However, if you only need basic date comparison, using the `Date` object will suffice.

When comparing dates, it is essential to keep in mind that the `Date` object in JavaScript stores dates as the number of milliseconds since January 1, 1970. This date is known as the Unix Epoch, and it serves as the starting point for calculating dates and time in many programming languages.

## See Also
- [MDN Web Docs: Date Object](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js](https://momentjs.com/)
- [js-Joda](https://js-joda.github.io/js-joda/)
- [Day.js](https://day.js.org/)