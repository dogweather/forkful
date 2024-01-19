---
title:                "Getting the current date"
html_title:           "Elm recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?

Getting the current date in JavaScript is the process of fetching real-time date information using built-in functions. Coders do it to keep track of when events happen, timestamp data, manage schedules, and compare times. 

## How To:

Simple and easy, here's your snippet:

```Javascript
let currentDate = new Date();
console.log(currentDate);
```

Your output will look similar to this:

```Javascript
Tue Sep 07 2021 11:01:57 GMT-0500 (Central Daylight Time)
```

Need just the date part? Here's how:

```Javascript
let currentDate = new Date();
let date = currentDate.getDate();   //Day of the month
let month = currentDate.getMonth(); //Month of the year
let year = currentDate.getFullYear();//Year

console.log(date + '/' + month + '/' + year);
```

This prints:

```Javascript
7/8/2021
```

## Deep Dive

Ditching the fluff, JavaScript dates have a storied history. Originally, the process was painful—confusing string manipulation and various date formats. Thankfully, JavaScript matured. We hopped on the fast track with the native `Date` object, standardized in ECMAScript 1st edition (1997).

If `Date` isn't your thing, alternatives exist. Libraries like Moment.js or date-fns come equipped with more powerful date-time functionalities, easier manipulation, and formatting. But remember, they come at a cost—additional dependencies and potential bloat to your application. 

Now, about the `Date` object. It works by storing the number of milliseconds since the Unix Epoch (January 1, 1970). Every `Date` method you use is a wrapper around this value, converting it into something human-readable or manipulable.

## See Also

Want more details? Dive deeper with these resources:

- [MDN Web Docs - JavaScript Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [JavaScript Info - Date and Time](https://javascript.info/date)
- [Moment.js](https://momentjs.com/)
- [date-fns - Modern JavaScript Date Utility Library](https://date-fns.org/)