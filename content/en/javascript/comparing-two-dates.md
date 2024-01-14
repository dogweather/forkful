---
title:    "Javascript recipe: Comparing two dates"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to compare two dates in Javascript but weren't sure how to do it? Maybe you want to check if a certain date is before or after another date, or if they are exactly the same. Comparing dates is a common task in Javascript programming and knowing how to do it can save you time and effort. In this blog post, we will explore the various methods and techniques for comparing two dates in Javascript.

## How To

When working with dates in Javascript, it is important to understand how they are represented. Dates are stored as numbers, specifically the number of milliseconds since January 1, 1970, known as the "epoch" or "unix time". This means that we can easily compare two dates using mathematical operators, such as greater than or less than.

To start, let's initialize two Date objects, representing two different dates:

```Javascript
let date1 = new Date("December 25, 2020");
let date2 = new Date("January 1, 2021");
```

To compare these two dates, we can use the greater than, less than, or equal to operators, as shown below:

```Javascript
console.log(date1 > date2); // false
console.log(date1 < date2); // true
console.log(date1 === date2); // false
```

Another useful method for comparing dates is using the `getTime()` method. This returns the number of milliseconds since the epoch, which can then be compared using mathematical operators.

```Javascript
console.log(date1.getTime() < date2.getTime()); // true 
```

In addition, the `getTime()` method also allows for more precise comparisons, such as checking if a date is before or after a specific time in milliseconds.

```Javascript
console.log(date1.getTime() < 1609237200000); // true - 1609237200000 represents January 1, 2021 at midnight in milliseconds
```

Lastly, there are also built-in methods like `Date.UTC()` and `Date.parse()` that can be used for comparing dates in a specific format. These methods also return the number of milliseconds since the epoch, making them useful for comparisons.

```Javascript
console.log(Date.UTC(2020, 11, 25) === Date.UTC(2020, 11, 25)); // true 
```

## Deep Dive

While comparing dates in Javascript may seem straightforward, there are some important considerations to keep in mind. One major issue is the possibility of time zone differences. When working with dates, it is important to always specify the time zone to avoid any discrepancies. This can be done using the `getTimezoneOffset()` method.

Another potential issue is leap years. Dates like February 29th can cause problems when comparing dates, as some years may have a leap day while others do not. The `getFullYear()` method can help with this by returning the four-digit year, allowing for accurate comparisons.

## See Also

To learn more about working with dates in Javascript, check out the following resources:

- [MDN Web Docs - Working with Dates](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [W3Schools - Javascript Dates](https://www.w3schools.com/js/js_dates.asp)
- [JavaScript.info - Date and Time](https://javascript.info/date)