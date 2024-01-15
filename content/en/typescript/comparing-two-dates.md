---
title:                "Comparing two dates"
html_title:           "TypeScript recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why

Comparing dates is a common task in web development, especially when working with user input or data from APIs. Whether you need to check if a date is in the future or past, or simply want to sort a list of dates, having the ability to compare them is crucial. Thankfully, TypeScript provides different methods for comparing dates, making this process much easier.

## How To

To compare two dates in TypeScript, you can use the following methods and operators.

### Using the greater than (>) and less than (<) operators

One way to compare dates is to use the greater than and less than operators. These operators compare the milliseconds of the dates, making it easy to determine which one is earlier or later.

```TypeScript
const date1: Date = new Date("2021-01-01");
const date2: Date = new Date("2021-01-15");

console.log(date1 < date2); // Output: true
console.log(date1 > date2); // Output: false
```
In this example, we create two Date objects and use the < and > operators to compare them. As expected, date1 is earlier than date2, so date1 < date2 returns true.

### Using the getTime() method

Another way to compare dates is by using the getTime() method. This method returns the number of milliseconds since January 1, 1970, making it easy to compare two dates without worrying about the time zone.

```TypeScript
const date1: Date = new Date("2021-01-01");
const date2: Date = new Date("2021-01-15");

console.log(date1.getTime() < date2.getTime()); // Output: true
console.log(date1.getTime() > date2.getTime()); // Output: false
```

### Using the getTimezoneOffset() method

If you want to compare dates while taking into account the timezone, you can use the getTimezoneOffset() method. This method returns the difference, in minutes, between the local time and UTC time. To compare two dates by their local time, you can subtract the getTimezoneOffset() values from their respective getTime() values.

```TypeScript
const date1: Date = new Date("2021-01-01");
const date2: Date = new Date("2021-01-15");

console.log(date1.getTime() + date1.getTimezoneOffset() * 60 * 1000 < date2.getTime() + date2.getTimezoneOffset() * 60 * 1000); 
//Output: true
```

## Deep Dive

When it comes to comparing dates, there are a few things to keep in mind. First, be aware of the time zone in which the dates are being inputted or retrieved. This can affect the comparison results if not taken into account. Additionally, keep in mind that when using the getTime() method, the values are measured in milliseconds, so any rounding errors could lead to incorrect results.

## See Also

- [TypeScript documentation on Date objects](https://www.typescriptlang.org/docs/handbook/date-and-time.html)
- [MDN web docs on comparing dates](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Stack Overflow thread on comparing dates in TypeScript](https://stackoverflow.com/questions/39951883/what-is-the-best-way-to-compare-dates-in-typescript)