---
title:                "Parsing a date from a string"
html_title:           "TypeScript recipe: Parsing a date from a string"
simple_title:         "Parsing a date from a string"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Parsing a date from a string means taking a string, which is just a sequence of characters, and converting it into a date object that can be manipulated and understood by the computer. Programmers do this to handle various date formats and to perform operations such as sorting and comparing dates.

## How to:
```TypeScript
//Using built-in Date constructor and toString method
const dateString = 'December 1, 2021'; 
const date = new Date(dateString);
console.log(date.toString());  //Output: Wed Dec 01 2021 00:00:00 GMT-0500 (Eastern Standard Time)

//Using Moment.js library 
import * as moment from 'moment';

const dateString = '12/01/2021';
const date = moment(dateString, 'MM/DD/YYYY');
console.log(date.format('dddd, MMMM DD YYYY')); //Output: Wednesday, December 01 2021 
```

## Deep Dive:
Parsing dates from strings has been an ongoing challenge for programmers, as dates can be represented in various formats and can also include different time zones and local conventions. In the past, developers had to write their own custom parsing logic which was not only time-consuming but also prone to errors. With the advent of libraries such as Moment.js, this task has become much easier and more accurate. Other alternatives to Moment.js include date-fns and Luxon. Under the hood, these libraries use complex parsing algorithms that take into account factors such as regional settings and daylight savings time.

## See Also:
- [Moment.js](https://momentjs.com/)
- [date-fns](https://date-fns.org/)
- [Luxon](https://moment.github.io/luxon/index.html)