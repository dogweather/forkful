---
title:                "Parsing a date from a string"
html_title:           "C recipe: Parsing a date from a string"
simple_title:         "Parsing a date from a string"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing a date from a string is about converting text into a date object that a computer can understand and process. Programmers do it to manipulate, calculate or compare dates received in text format.

## How to:

The simplest way to parse a date from a string in TypeScript is to use JavaScript's built-in Date object constructor. Here's an example:

```TypeScript
let myDate: Date = new Date("2022-05-11T10:20:30Z");
console.log(myDate.toString());
```
The output is the local date and time:

```JavaScript
Wed May 11 2022 12:20:30 GMT+0200 (Central European Summer Time)
```
You can create date objects from string formats not recognized by JavaScript using moment.js. First, install it using `npm install moment`, then use it like this:

```TypeScript
import * as moment from 'moment';

let myDate = moment("11-05-2022", "DD-MM-YYYY");
console.log(myDate.toString());
```
The output:

```JavaScript
Wed May 11 2022 00:00:00 GMT+0200
```
## Deep Dive

Parsing a date from a string has been a common task since the early days of programming. Over time, many methods have been developed, adapted to language capabilities and user needs.

In JavaScript (and thus TypeScript), the Date object has been there since its inception. The Date constructor tries to parse many common string formats, but can return inconsistent results due to variations in string formats and locale dependencies.

That's where libraries like moment.js come in handy. Moment.js offers more flexible parsing and formatting options, and local time zone support. However, it’s significantly larger than many alternative libraries, which might make it less suitable for lightweight web projects.

An alternative to moment.js can be date-fns or day.js. These libraries are more lightweight and modular, allowing you to import only the functions you need.

## See Also

- [Date and Time Strings (JavaScript)](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/parse)
- [Format](https://momentjs.com/docs/#/parsing/string-format/) and [Parse](https://momentjs.com/docs/#/parsing/string/) in Moment.js
- [Parse](https://date-fns.org/v2.14.0/docs/parse) in date-fns
- [Parse](https://day.js.org/docs/en/parse/string-format) in Day.js.