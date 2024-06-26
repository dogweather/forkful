---
date: 2024-01-20 17:36:52.217820-07:00
description: 'How to: JavaScript has built-in methods to convert dates to strings.
  Here''s how to use them.'
lastmod: '2024-03-13T22:45:00.444326-06:00'
model: gpt-4-1106-preview
summary: JavaScript has built-in methods to convert dates to strings.
title: Converting a date into a string
weight: 28
---

## How to:
JavaScript has built-in methods to convert dates to strings. Here's how to use them:

```javascript
const now = new Date();

// toLocaleString() - local format
console.log(now.toLocaleString()); // '4/1/2023, 12:00:00 PM' 

// toString() - standard format
console.log(now.toString()); // 'Sat Apr 01 2023 12:00:00 GMT+0100 (Central European Standard Time)'

// toISOString() - ISO format (great for databases/network)
console.log(now.toISOString()); // '2023-04-01T11:00:00.000Z'
```

## Deep Dive
Back in the day, date to string conversion was a mess—no standards, just a bunch of custom functions. Thankfully, ECMAScript stepped in, standardizing the Date object in ES5 and adding the super handy `toISOString()` in ES5.1.

Alternatives to the native methods include libraries like `moment.js` and `date-fns`, which offer more control and time zone handling, but they add to your project's size.

Under the hood, when you call a date-to-string method, JavaScript interacts with the system's locale settings and timezone information to generate the string output. In contrast, `toISOString()` always returns a UTC time (the 'Z' stands for 'Zulu time' or zero offset from UTC).

## See Also
- [MDN Web Docs – Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [ISO 8601 Date and time format](https://www.iso.org/iso-8601-date-and-time-format.html)
- [date-fns](https://date-fns.org/)
