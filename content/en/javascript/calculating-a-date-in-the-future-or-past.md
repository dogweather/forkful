---
title:                "Javascript recipe: Calculating a date in the future or past"
simple_title:         "Calculating a date in the future or past"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why

Have you ever found yourself trying to plan for an event or project that will take place in the future? Perhaps you need to know the date for a birthday or an important deadline. In these situations, having the ability to calculate a date in the future or past can be incredibly useful.

## How To

The good news is that Javascript offers a simple way to achieve this. The Date object, along with a few built-in methods, allows us to easily manipulate dates. Let's take a look at some code examples:

```
// Create a new Date object with the current date
let currentDate = new Date();

// Calculate a date 5 days from now
let futureDate = new Date();
futureDate.setDate(currentDate.getDate() + 5);

// Calculate a date 3 months in the past
let pastDate = new Date();
pastDate.setMonth(currentDate.getMonth() - 3);
```

In the first example, we use the `getDate()` method to get the current day, and then add 5 to it to get the future date. Similarly, in the second example, we use the `getMonth()` method to get the current month, and then subtract 3 to get the past date.

We can also use the `getFullYear()` and `getMinutes()` methods to get the current year and minutes, respectively. With these methods, we can easily calculate any date in the future or past.

## Deep Dive

Behind the scenes, dates in Javascript are represented in milliseconds since January 1, 1970. This means that we can also do calculations using milliseconds.

```
// Create a new Date object with the current date
let currentDate = new Date();

// Calculate a date 10 hours from now
let futureDate = new Date(currentDate.getTime() + (10 * 60 * 60 * 1000));
```

In the above example, we use the `getTime()` method to get the current date in milliseconds, then add 10 hours (converted to milliseconds) to it. This gives us a date 10 hours in the future.

## See Also

- [MDN Web Docs - Date objects](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [W3Schools - Date methods](https://www.w3schools.com/js/js_date_methods.asp)
- [Stack Overflow - How to calculate date in Javascript](https://stackoverflow.com/questions/563406/add-days-to-javascript-date)