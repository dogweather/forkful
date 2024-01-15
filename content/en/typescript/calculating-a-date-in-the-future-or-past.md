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

## Why

Calculating dates in the future or past can be a useful tool for a variety of applications. Whether you need to schedule an event, calculate timelines, or simply keep track of time, being able to accurately calculate dates is a valuable skill in any programming language.

## How To

To calculate a date in the future or past with TypeScript, we first need to import the `Date` class from the standard library. This class allows us to work with dates and perform various operations on them. Here's an example of how we can create a new `Date` object and set it to a specific date:

```TypeScript
import { Date } from 'datejs';

// Creating a new Date object for March 15, 2021
let date = new Date(2021, 2, 15);
```

Once we have our `Date` object, we can use the built-in methods to perform calculations on it. To add or subtract days, we use the `addDays()` and `subDays()` methods respectively. Here's an example of how we can add 10 days to our previously created date:

```TypeScript
import { Date } from 'datejs';

// Creating a new Date object for March 15, 2021
let date = new Date(2021, 2, 15);

// Adding 10 days to the date
date.addDays(10);

console.log(date.toString()); // Output: Thu Mar 25 2021 00:00:00 GMT+0530 (India Standard Time)
```

We can also add or subtract months and years using the `addMonths()` and `subMonths()` methods, and `addYears()` and `subYears()` methods respectively. It is important to note that these methods modify the original date object, so if you want to keep the original date, make sure to make a copy of it before performing any calculations.

## Deep Dive

Behind the scenes, the way TypeScript calculates dates is relatively straightforward. Each date is represented by a number, known as a timestamp, which is the number of milliseconds that have elapsed since January 1, 1970. By manipulating this timestamp, we can add or subtract a specific amount of time to a given date.

Additionally, the `Date` class in TypeScript also takes into account timezones and daylight saving time. It automatically adjusts the timestamp according to the timezone set on the system, ensuring accurate calculations regardless of the user's location.

## See Also

- [TypeScript Playground](https://www.typescriptlang.org/play): An online sandbox to test and run TypeScript code.
- [Date.js Documentation](https://date.js.org/docs/): Official documentation for the `Date` class and its methods.
- [Moment.js](https://momentjs.com/): Another popular JavaScript library for working with dates and times.