---
title:                "Calculating a date in the future or past"
html_title:           "Javascript recipe: Calculating a date in the future or past"
simple_title:         "Calculating a date in the future or past"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why

Calculating dates in the future or past may seem like a daunting task, but it can actually be quite useful in many programming scenarios. Being able to determine a future date, for example, can help with scheduling events and tasks, while calculating a past date can assist with data analysis.

## How To

To calculate a date in the future or past, we can use the `Date()` object in Javascript. Let's look at some examples below:

First, we create a new `Date` object and assign it to a variable called `today`:
```Javascript
let today = new Date();
```

We can then use the `setDate()` method to set the date to a specific day by passing in a numerical value:
```Javascript
today.setDate(10); //this will set the date to the 10th of the current month
```

To calculate a future date, we can use the `setDate()` method and pass in the desired number of days to add:
```Javascript
today.setDate(today.getDate() + 7); //this will add 7 days to the current date
```

Similarly, to calculate a past date, we can use the `setDate()` method and pass in a negative number of days:
```Javascript
today.setDate(today.getDate() - 14); //this will subtract 14 days from the current date
```

We can also use the `setFullYear()` method to change the year of the date object:
```Javascript
today.setFullYear(2022); //this will set the year to 2022
```

And finally, we can retrieve the updated date information using the `getDate()`, `getMonth()`, and `getFullYear()` methods:
```Javascript
let day = today.getDate();
let month = today.getMonth();
let year = today.getFullYear();
console.log(`${month+1}/${day}/${year}`); //outputs "8/24/2022" (assuming today's date is 8/24/2021)
```

## Deep Dive

Behind the scenes, the `Date()` object in Javascript stores the date and time as the number of milliseconds that have elapsed since January 1, 1970. This is known as the Unix Epoch and is the basis for date and time calculations in many programming languages.

When using the `getDate()` and `setDate()` methods, it is important to note that the day values are zero-based. This means that January is represented as 0, February as 1, and so on. Therefore, when setting a future date, we need to add 1 to the desired month value.

Additionally, the `setFullYear()` method accepts a four-digit year, while the `getFullYear()` method returns a four-digit value. Keep this in mind when working with dates from different date systems.

## See Also

- [Javascript Date object documentation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Unix time explanation](https://www.epochconverter.com/)