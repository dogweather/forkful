---
title:                "TypeScript recipe: Comparing two dates"
programming_language: "TypeScript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why

Comparing dates is a crucial part of any programming language, including TypeScript. It allows developers to check for past or future dates, calculate time differences, and perform complex date operations. In this blog post, we will explore how to compare two dates in TypeScript with practical coding examples.

## How To

To start with, TypeScript has a built-in Date object that represents a single moment in time. To compare two dates, we need to create two Date objects with different dates and use the available methods to compare them.

```
TypeScript
let date1 = new Date(2021, 5, 1); // June 1st, 2021
let date2 = new Date(2021, 7, 30); // August 30th, 2021
```

Now, we can use the Date methods to compare these two dates. The most commonly used methods are `getTime()` and `toDateString()`. The `getTime()` method returns the milliseconds since January 1, 1970, while the `toDateString()` method returns the date in the form of a string.

```
TypeScript
console.log(date1.getTime()); // 1622505600000
console.log(date2.getTime()); // 1630329600000
console.log(date1.toDateString()); // Tue Jun 01 2021
console.log(date2.toDateString()); // Mon Aug 30 2021
```

We can use these methods to compare the dates based on milliseconds or string values. For example, to check if `date1` is before `date2`, we can use the `getTime()` method as follows:

```
TypeScript
if (date1.getTime() < date2.getTime()) {
    console.log("date1 is before date2");
}
```

Similarly, we can compare the dates based on string values using the `toDateString()` method:

```
TypeScript
if (date1.toDateString() < date2.toDateString()) {
    console.log("date1 is before date2");
}
```

Apart from these methods, TypeScript also has other useful methods such as `getTimezoneOffset()`, `getFullYear()`, `getMonth()`, `getDate()`, etc. These methods can be used to compare specific date components, such as the year, month, or day.

## Deep Dive

When comparing dates, it is essential to keep in mind that dates are stored as objects in TypeScript. So comparing them using logical operators like `>, <, ===` might not give the desired output. Instead, we need to use the methods discussed above to accurately compare dates.

Also, it is vital to note that when creating Date objects, the month is zero-based, i.e., January is represented as 0, February as 1, and so on. This can often lead to confusion, so it is recommended to use the `getFullYear()`, `getMonth()`, and `getDate()` methods to construct Date objects.

Lastly, when comparing dates, it is essential to consider time zones. The `getTime()` method returns the milliseconds since January 1, 1970, in UTC time, while the `toDateString()` method returns the date in local time. So, it is recommended to either convert both dates to UTC or both to local time before comparing them.

## See Also

- [TypeScript Date Object](https://www.typescriptlang.org/docs/handbook/2/classes.html#date)
- [Date Methods in TypeScript](https://www.w3schools.com/jsref/jsref_obj_date.asp)

By now, you should have a good understanding of how to compare two dates in TypeScript. With the help of the Date object and its methods, you can easily compare dates and perform complex date operations in your TypeScript code. Happy coding!