---
title:                "TypeScript recipe: Calculating a date in the future or past"
simple_title:         "Calculating a date in the future or past"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why

As developers, we often need to work with dates in our code to perform various tasks. One common task is calculating a date in the future or past. This could be for tasks such as scheduling reminders, generating reports, or tracking user activity. Being able to accurately calculate dates is essential in creating efficient and functional programs.

## How To

Calculating a date in the future or past may seem like a daunting task, but with TypeScript, it can be done easily and accurately. Let's take a look at a few examples of how to calculate a date using TypeScript.

First, we will need to import the `date-fns` library in our code. This library provides various functions for working with dates and times.

```TypeScript
import { addDays } from 'date-fns';
```

To calculate a date in the future or past, we will use the `addDays` function. This function takes in two parameters - the date we want to start with, and the number of days we want to add (or subtract). Let's look at a simple example:

```TypeScript
// Today's date
const currentDate = new Date();

// Calculate date 5 days in the future
const futureDate = addDays(currentDate, 5);

// Print the future date in the console
console.log(futureDate); // Output: Sun Aug 08 2021 00:00:00 GMT+0530 (India Standard Time)
```

As you can see, by using the `addDays` function, we were able to easily calculate a date 5 days in the future. Similarly, we can also calculate dates in the past by passing in a negative number as the second parameter.

```TypeScript
// Today's date
const currentDate = new Date();

// Calculate date 3 days in the past
const pastDate = addDays(currentDate, -3);

// Print the past date in the console
console.log(pastDate); // Output: Thu Jul 29 2021 00:00:00 GMT+0530 (India Standard Time)
```

Apart from `addDays`, the `date-fns` library also provides functions to add or subtract months, years, hours, minutes, and seconds from a given date. These functions work in a similar way to `addDays` and can be used according to our needs.

## Deep Dive

Behind the scenes, the `addDays` function uses the JavaScript `Date` object. The `Date` object represents a single moment in time and can be created with specific date and time values.

When we pass a date parameter to the `addDays` function, it uses the `getTime()` method of the `Date` object to get the number of milliseconds passed since January 1, 1970, 00:00:00 UTC. It then adds (or subtracts) the specified number of days, converts it back to a `Date` object, and returns it.

## See Also

For a more in-depth understanding of the `date-fns` library and working with dates, check out the official documentation and some helpful resources below:

- [date-fns Documentation](https://date-fns.org/docs/)
- [Exploring Javascript's Object-Oriented Date and Time API - Smashing Magazine](https://www.smashingmagazine.com/2018/04/javascript-date-and-time-manipulation/)
- [Moment.js vs date-fns: Which Is The Right Library To Use? - Scotch.io](https://scotch.io/tutorials/momentjs-vs-date-fns-which-is-reduxs-momentjs-replacement)