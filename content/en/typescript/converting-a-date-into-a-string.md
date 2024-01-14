---
title:                "TypeScript recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why

If you're working with dates in TypeScript, you may have encountered the need to convert a date object into a string. This could be for display purposes or for passing data to an external API. Whatever the reason, knowing how to convert a date into a string in TypeScript can come in handy.

## How To

Converting a date into a string in TypeScript involves using the `toString()` method. This method converts a Date object into a string in the specified format. Here's an example:

```TypeScript
let currentDate: Date = new Date();
console.log(currentDate.toString());
```

This will output the current date and time in the default format, which may vary depending on your location and browser settings. For example:

```
Sun Aug 29 2021 21:11:03 GMT-0400 (Eastern Daylight Time)
```

You can also specify a specific format for the date string by using the `toLocaleDateString()` method. This method takes in parameters for the locale and options. Here's an example:

```TypeScript
let currentDate: Date = new Date();
console.log(currentDate.toLocaleDateString("en-US", {year: "numeric", month: "long", day: "numeric"}));
```

This will output the current date in a long format with the month spelled out. For example:

```
August 29, 2021
```

## Deep Dive

While the `toString()` and `toLocaleDateString()` methods are the most common ways of converting a date into a string in TypeScript, there are other options available as well. One alternative is to use a library like Moment.js, which offers more flexibility and options for formatting date strings.

Another important aspect to consider is time zones. By default, the Date object in TypeScript will use the local time zone of the device it is running on. However, you can also specify a specific time zone using the `setTimezoneOffset()` method before converting the date into a string.

## See Also

- [Moment.js](https://momentjs.com/)
- [Date object reference](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)