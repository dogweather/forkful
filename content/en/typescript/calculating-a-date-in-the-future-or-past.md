---
title:                "TypeScript recipe: Calculating a date in the future or past"
programming_language: "TypeScript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Why: Calculating Dates in the Future or Past

Have you ever needed to calculate a date in the future or past in your TypeScript program? Perhaps you are creating a booking system and need to display dates for booking availability. Or maybe you are creating a countdown timer and need to calculate a future date to countdown towards. Whatever the reason may be, knowing how to accurately calculate dates in the future or past is a useful skill for any TypeScript programmer.

# How To: Coding Examples and Sample Output

Calculating dates in the future or past may seem like a complex task, but with TypeScript, it's actually quite simple. Let's take a look at some coding examples to see just how easy it can be.

## Calculating a Date in the Future

To calculate a date in the future, we can use the `Date` class in TypeScript. First, we need to create a new `Date` object with the current date as its value. Then, we can use the `setDate()` method to set the date to a future date by providing the number of days we want to add to the current date. Finally, we can use the `getFullYear()`, `getMonth()`, and `getDate()` methods to get the year, month, and day respectively.

Here's an example of calculating a date 10 days into the future:

```TypeScript
const currentDate = new Date();
currentDate.setDate(currentDate.getDate() + 10);

const year = currentDate.getFullYear();
const month = currentDate.getMonth() + 1; // Note: Month is zero-indexed
const day = currentDate.getDate();

console.log(`The date 10 days from now is: ${day}/${month}/${year}`);
// Output: The date 10 days from now is: 20/7/2021
```

## Calculating a Date in the Past

Calculating a date in the past follows a similar process as calculating a date in the future. However, this time we will use the `setDate()` method to subtract the number of days from the current date. We can also use the `getFullYear()`, `getMonth()`, and `getDate()` methods to get the year, month, and day respectively for the past date.

Let's see an example of calculating a date 5 days in the past:

```TypeScript
const currentDate = new Date();
currentDate.setDate(currentDate.getDate() - 5);

const year = currentDate.getFullYear();
const month = currentDate.getMonth() + 1; 
const day = currentDate.getDate();

console.log(`The date 5 days ago was: ${day}/${month}/${year}`);
// Output: The date 5 days ago was: 10/7/2021
```

# Deep Dive: The Date Object

The `Date` class in TypeScript is a built-in object that represents a specific moment in time. It has various methods for getting and setting different date components, such as the year, month, day, hour, minute, and second. It also has methods for comparing dates and working with timezones.

To learn more about the `Date` class and its methods, check out the TypeScript documentation for dates [here](https://www.typescriptlang.org/docs/handbook/2/objects.html#date).

# See Also

- [Using Date and Time in JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [TypeScript Handbook: Date](https://www.typescriptlang.org/docs/handbook/2/objects.html#date)
- [How to Work with Dates and Time in TypeScript](https://www.sitepoint.com/working-with-dates-and-time-in-typescript/)

Now that you have a better understanding of how to calculate dates in the future or past with TypeScript, put your knowledge into practice and start incorporating dates into your next project! Happy coding!