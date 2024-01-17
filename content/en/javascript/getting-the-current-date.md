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

## What & Why?

Getting the current date is a way for programmers to keep track of time and date in their code. It is essential for creating time-based functionalities such as scheduling tasks or logging events.

## How to:

To get the current date using Javascript, we can use the built-in `Date()` object. This object has various methods for retrieving different components of the date. For example:

```Javascript
const currentDate = new Date()

console.log(currentDate.getFullYear()) // outputs current year
console.log(currentDate.getMonth()) // outputs current month
console.log(currentDate.getDate()) // outputs current day of the month
console.log(currentDate.getHours()) // outputs current hour
console.log(currentDate.getMinutes()) // outputs current minutes
console.log(currentDate.getSeconds()) // outputs current seconds
```

We can also get the current date and time in a specific format using the `toLocaleDateString()` method. For example:

```Javascript
const formattedDate = currentDate.toLocaleDateString("en-US")

console.log(formattedDate) // outputs current date in MM/DD/YYYY format
```

## Deep Dive:

Getting the current date is not a new concept in programming. In fact, it has been a part of languages like Java and C++ for a long time. However, Javascript's `Date()` object was introduced in the ECMAScript 1.0 standard in 1997.

There are also alternative methods for getting the current date in Javascript. One popular method is using libraries such as Moment.js or Luxon, which provide more readable and flexible ways of handling dates.

Internally, the `Date()` object in Javascript stores the date and time as the number of milliseconds passed since January 1, 1970, 00:00:00 UTC. This value is referred to as the Unix timestamp and can be retrieved using the `getTime()` method.

## See Also:

- [MDN web docs on the Date object in Javascript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js library documentation](https://momentjs.com/docs/)
- [Luxon library documentation](https://moment.github.io/luxon/)