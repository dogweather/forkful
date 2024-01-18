---
title:                "स्ट्रिंग से तारीख को पार्स करना"
html_title:           "TypeScript: स्ट्रिंग से तारीख को पार्स करना"
simple_title:         "स्ट्रिंग से तारीख को पार्स करना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

What & Why?
Parsing a date from a string is the process of taking a date in string form and converting it into a Date object. This is a common task for programmers, as it allows them to manipulate and work with dates in their code. It is especially useful when dealing with user input or data from an external source.

How to:
```TypeScript
const dateStr: string = "05/15/2021";
const date: Date = new Date(dateStr);
console.log(date);

// Output: Sat May 15 2021 00:00:00 GMT+0530 (India Standard Time)
```
In this example, we first declare a variable named `dateStr` and assign it a string value representing a date. Then, we create a new Date object by passing in the `dateStr` variable, which is automatically parsed into a Date object. Finally, we log the date object to the console, which displays the parsed date in the desired format.

Deep Dive:
Parsing dates from strings has been a common practice in programming for a long time. It became even more relevant with the rise of web development, as web applications often depend on user input that includes dates. While there are alternative libraries available for parsing dates, the native Date object in TypeScript is the most commonly used and reliable method.

See Also:
- [TypeScript Date documentation](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#date)
- [Moment.js - popular alternative library for working with dates](https://momentjs.com/)