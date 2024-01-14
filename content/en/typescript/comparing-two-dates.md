---
title:                "TypeScript recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why
Comparing dates is a common task in programming, especially when dealing with time-sensitive data or event scheduling. By comparing two dates, we can determine which date is earlier or later, calculate the time difference between them, and perform various operations.

## How To
To compare two dates in TypeScript, we can use the built-in `Date` object. This object represents a single moment in time and provides various methods for comparing dates.

```TypeScript
// Create two date objects
let date1 = new Date(2021, 5, 14); // June 14, 2021
let date2 = new Date(2020, 2, 23); // March 23, 2020

// Compare dates using the `getTime()` method
if (date1.getTime() > date2.getTime()) {
    console.log("Date1 is later than date2");
} else if (date1.getTime() < date2.getTime()) {
    console.log("Date1 is earlier than date2");
} else {
    console.log("Both dates are the same");
}

// Output: Date1 is later than date2
```

We can also use the comparison operators (`<`, `>`, `<=`, `>=`) to compare dates. These operators use the `getTime()` method behind the scenes, so the results are the same.

```TypeScript
// Create two date objects
let date1 = new Date(2021, 5, 14); // June 14, 2021
let date2 = new Date(2021, 5, 20); // June 20, 2021

// Compare dates using comparison operators
if (date1 < date2) {
    console.log("Date1 is earlier than date2");
} else if (date1 > date2) {
    console.log("Date1 is later than date2");
} else {
    console.log("Both dates are the same");
}

// Output: Date1 is earlier than date2
```

To calculate the time difference between two dates, we can use the `getTime()` method and perform some simple calculations with the returned values. For example, to find the number of days between two dates, we can divide the time difference by the number of milliseconds in a day (86400000).

```TypeScript
// Create two date objects
let date1 = new Date(2021, 5, 14); // June 14, 2021
let date2 = new Date(2021, 4, 14); // May 14, 2021

// Calculate the time difference in milliseconds
let timeDiff = date1.getTime() - date2.getTime();

// Calculate the number of days
let days = Math.floor(timeDiff / 86400000);

console.log(`The time difference is ${days} days`);

// Output: The time difference is 31 days
```

## Deep Dive
The `Date` object in TypeScript is based on the JavaScript `Date` object, which has some quirks and limitations. For example, the `Date` object does not accurately represent dates before 1970, and it does not account for timezones. To overcome these limitations, we can use libraries like Moment.js or Day.js, which provide more robust date manipulation capabilities.

Additionally, when comparing dates, it is essential to consider the timezone of the user. Converting dates to UTC before comparing can help avoid discrepancies.

## See Also
- [MDN web docs - Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js](https://momentjs.com/)
- [Day.js](https://day.js.org/)