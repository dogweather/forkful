---
title:    "TypeScript recipe: Comparing two dates"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Why

As a developer, you may encounter situations where you need to compare two dates in your TypeScript code. This could be for tasks such as sorting a list of dates or validating user input in a form. Understanding how to properly compare dates in TypeScript can save you time and headaches in your development process.

## How To

To compare two dates in TypeScript, we can use the `Date` object and its associated methods. Let's take a look at some code examples to see how this can be done.

```TypeScript
const date1 = new Date("2021-01-01");
const date2 = new Date("2020-12-01");

// Using `getTime()` method
console.log(date1.getTime() > date2.getTime()); // Output: true

// Using comparison operators
console.log(date1 > date2); // Output: true
```

In the first example, we are using the `getTime()` method to get the number of milliseconds since January 1, 1970 for each date. We then compare these values to determine which date is larger.

Alternatively, in the second example, we use traditional comparison operators which also work with `Date` objects since they are converted into their numeric representations.

Let's look at another example where we compare the month and year of two dates.

```TypeScript
const date1 = new Date("2021-01-01");
const date2 = new Date("2021-01-05");

// Using `getMonth()` and `getFullYear()` methods
console.log(date1.getMonth() === date2.getMonth()); // Output: true
console.log(date1.getFullYear() === date2.getFullYear()); // Output: true
```

In this example, we check if the months and years of both dates match using the `getMonth()` and `getFullYear()` methods. These methods return the month (0-11) and year of the `Date` object, respectively.

## Deep Dive

When comparing dates, it's important to keep in mind that the timezones of the dates may affect the results. For example, if you compare a date in your local timezone to a date in a different timezone, the results may not be accurate.

Another thing to note is that when using comparison operators, the `=` sign is included. This means that if we want to exclude the `=` sign, we need to explicitly state it in our code.

For more complex date comparisons, we can also use libraries such as moment.js which provide more advanced methods for manipulating and comparing dates in JavaScript and TypeScript.

## See Also

Here are some helpful resources for further reading on comparing dates in TypeScript:

- [TypeScript `Date` object documentation](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-2.html#the-date-type)
- [Moment.js documentation](https://momentjs.com/docs/#/query/)

Now that you know how to compare dates in TypeScript, you can confidently handle situations where you need to compare dates in your code. Keep these tips in mind and continue to explore and practice to become a proficient TypeScript developer. Happy coding!