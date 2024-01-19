---
title:                "Getting the current date"
html_title:           "Elm recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?

Getting the current date programmatically, in TypeScript or any other language, is about retrieving the real-world, present-time date from your system. This can help to keep track of when some data was recorded or processed.

## How to:

The simplest way to get the current Date and Time in TypeScript is:

```TypeScript
let current_date = new Date();
console.log(current_date);
```

This will output something like:

```shell
2022-05-03T15:37:22.875Z
```

If you just need the date, you can extract it using the `toDateString()` function:

```TypeScript
let current_date = new Date().toDateString();
console.log(current_date);
```

This will output the date in this format:

```shell
Tue May 03 2022
```

## Deep Dive:

The `Date` object in JavaScript (which TypeScript extends) really came to life with ECMAScript 1 (1997). It provides the tools to work with date and time, which are essential in almost any application you can think of. 

There are built-in JavaScript methods for getting the full year, month, day, and time. These can be combined in various ways depending on what's needed.

An alternative to the JavaScript Date object is libraries such as Moment.js, which provides more comprehensive and flexible functionality. However, for simple date tasks like getting the current date, using the built-in JavaScript methods will suffice, plus there's no need to add extra dependencies into your project. 

The implementation details of how the Date object gets the system's current date can vary across different runtime environments, but today most of them will refer to the system's internal clock.

## See Also:

For more detailed information on the JavaScript Date object, visit [Mozilla Developer Network's Date Documentation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)

For a comprehensive library for handling date and time, check out [Moment.js](https://momentjs.com/).