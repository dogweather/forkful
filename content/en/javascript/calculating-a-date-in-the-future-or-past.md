---
title:                "Javascript recipe: Calculating a date in the future or past"
programming_language: "Javascript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why

Calculating dates in the future or past can be a useful tool in programming. It allows for dynamic date-based functionality, such as setting reminders or scheduling tasks. By understanding how to calculate dates in Javascript, you can add a new level of customization and efficiency to your code.

## How To

Calculating dates in Javascript can be done using the built-in `Date` object and its methods. Here is an example code:

```Javascript
// Create a new Date object
var currentDate = new Date();
// Get the current day, month, and year
var currentDay = currentDate.getDate();
var currentMonth = currentDate.getMonth();
var currentYear = currentDate.getFullYear();

// Calculate a date 7 days in the future
var futureDate = new Date(currentYear, currentMonth, currentDay + 7);
// Get the day, month, and year of the future date
var futureDay = futureDate.getDate();
var futureMonth = futureDate.getMonth();
var futureYear = futureDate.getFullYear();

// Output the future date
console.log("In 7 days, the date will be: " + futureMonth + "/" + futureDay + "/" + futureYear);
```

Output:

```
In 7 days, the date will be: 8/23/2021
```

The above code uses the `getDate()`, `getMonth()`, and `getFullYear()` methods to retrieve the current day, month, and year from the `Date` object. Then, by creating a new `Date` object with a specified future date, we can retrieve the individual components of the future date and output it in the desired format.

## Deep Dive

The Date object also has other useful methods for calculating dates in the future or past. One such method is `setDate()`, which can be used to set a specific day of the month. For example, if we want to calculate a date one month in the future, we can use:

```Javascript
// Create a new Date object
var currentDate = new Date();
// Set the date to 1st of next month
currentDate.setDate(1 + currentDate.getDate());
// Get the day, month, and year of the future date
var futureDay = currentDate.getDate();
var futureMonth = currentDate.getMonth();
var futureYear = currentDate.getFullYear();

// Output the future date
console.log("One month from now, the date will be: " + futureMonth + "/" + futureDay + "/" + futureYear);
```

Output:

```
One month from now, the date will be: 9/9/2021
```

Additionally, the `setFullYear()` method can be used to set a specific year, while the `setMonth()` method can be used to set a specific month. By combining these methods with the `getDate()` method, the possibilities for calculating dates in the future or past are endless.

## See Also

- [MDN Date Object Documentation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [W3Schools Date Object Tutorial](https://www.w3schools.com/js/js_dates.asp)
- [Calculate future or past dates in Javascript](https://www.techiedelight.com/calculate-future-past-dates-javascript/)