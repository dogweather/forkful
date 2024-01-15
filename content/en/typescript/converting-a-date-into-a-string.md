---
title:                "Converting a date into a string"
html_title:           "TypeScript recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why

Dates and times are a crucial part of many applications, whether it's for displaying the current date, scheduling events, or tracking data. However, dates in their original form are not always user-friendly. That's why converting a date into a string is important as it allows for easier manipulation and display of dates in a readable format.

## How To

To convert a date into a string in TypeScript, we can use the `toString()` method. This method converts the date object into a string in a default format, which is the local date and time.

```TypeScript
let currentDate = new Date();
console.log(currentDate.toString());
// Output: Fri Nov 20 2020 22:09:49 GMT-0800 (Pacific Standard Time)
```

If we want to specify a different format for the string, we can use the `toLocaleString()` method. This method allows us to pass in parameters for the date, time, and timezone to customize the string output.

```TypeScript
let currentDate = new Date();
console.log(currentDate.toLocaleString('en-US', { dateStyle: 'full', timeStyle: 'long', timeZone: 'America/Los_Angeles' }));
// Output: Friday, November 20th, 2020 at 10:13:14 PM Pacific Standard Time
```

We can also use the `toDateString()` and `toTimeString()` methods to get just the date or time portion of the string, respectively.

```TypeScript
let currentDate = new Date();
console.log(currentDate.toDateString());
// Output: Fri Nov 20 2020
console.log(currentDate.toTimeString());
// Output: 22:15:20 GMT-0800 (Pacific Standard Time)
```

## Deep Dive

The `toString()` and `toLocaleString()` methods are both locale-sensitive, meaning they will output the date and time in the format specific to the user's timezone and language. This allows for a more user-friendly experience, as the date and time will be displayed in a familiar format.

In addition to the methods mentioned above, there are also options for custom date and time formatting using the `toLocaleDateString()` and `toLocaleTimeString()` methods. These methods allow us to pass in a format string to specify exactly how we want the date and time to be displayed.

```TypeScript
let currentDate = new Date();
console.log(currentDate.toLocaleDateString('en-US', { year: 'numeric', month: 'long', day: 'numeric' }));
// Output: November 20, 2020
console.log(currentDate.toLocaleTimeString('en-US', { hour: 'numeric', minute: 'numeric', hour12: true }));
// Output: 10:30 PM
```

## See Also

- [MDN Web Docs - Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [TypeScript Documentation - Date](https://www.typescriptlang.org/docs/handbook/date-and-time.html)