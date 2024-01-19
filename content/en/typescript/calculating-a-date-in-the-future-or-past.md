---
title:                "Calculating a date in the future or past"
html_title:           "TypeScript recipe: Calculating a date in the future or past"
simple_title:         "Calculating a date in the future or past"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Calculating Dates in TypeScript: An Intuitive Approach

## What & Why?
Calculating a date in the future or past is when you determine a specific date from a set point in time. Programmers frequently do it to deal with scheduling, data analytics, tracking user activities, event planning, and similar time-bound functionalities.

## How to:
In TypeScript, we can calculate future or past dates using in-built Date object manipulations. Here's how:

```TypeScript
let currentDate = new Date();
// Print the current date
console.log("Current Date:", currentDate);

//Calculate a date 5 days into the future
let futureDate = new Date();
futureDate.setDate(currentDate.getDate() + 5);
console.log("Future Date:", futureDate);

//Calculate a date 3 days in the past
let pastDate = new Date();
pastDate.setDate(currentDate.getDate() - 3);
console.log("Past Date:", pastDate);
```
The above snippet will calculate and print the current, future, and past dates.

## Deep Dive
Since the advent of Java in the '90s, handling dates has been a common task in programming. JavaScript (from which TypeScript is derived) has the Date object, originated from Java, offering powerful and versatile date/time functionalities.

There exist library alternatives like `moment.js` and `date-fns` providing extensive functionalities in manipulating dates and time. However, `moment.js` is considered a heavy library and is not recommended for projects sensitive to bundle size.

The `setDate()` method works by getting the current date with `getDate()` and adding or subtracting days appropriately. It considers edge cases: should the calculated date fall outside the current month, it seamlessly adjusts the month and year. 

## See Also
For more complex calculations or formats, considering libraries like [Moment.js](https://momentjs.com/) or [date-fns](https://date-fns.org/) might be beneficial. The JavaScript [MDN documentation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date) also covers additional, native Date methods and properties.