---
date: 2024-02-03 19:02:33.752386-07:00
description: "How to: In vanilla JavaScript, the `Date` object is used to work with\
  \ dates and times. Here\u2019s how you can get the current date and time."
lastmod: '2024-03-13T22:45:00.443414-06:00'
model: gpt-4-0125-preview
summary: In vanilla JavaScript, the `Date` object is used to work with dates and times.
title: Getting the current date
weight: 29
---

## How to:
In vanilla JavaScript, the `Date` object is used to work with dates and times. Here’s how you can get the current date and time:

```javascript
const currentDate = new Date();
console.log(currentDate); // Example output: Fri Apr 14 2023 12:34:56 GMT+0100 (British Summer Time)
```

To display only the date in a more user-friendly format, you can use methods like `toLocaleDateString()`:

```javascript
console.log(currentDate.toLocaleDateString()); // Example output: 4/14/2023
```

For more control over the format, third-party libraries like *Moment.js* or *date-fns* are very popular, though it's good to be aware that Moment.js is now considered a legacy project in maintenance mode.

Using *Moment.js*:

```javascript
const moment = require('moment'); // assuming Node.js or using a module bundler
const formattedDate = moment().format('YYYY-MM-DD');
console.log(formattedDate); // Example output: 2023-04-14
```

With *date-fns*, which emphasizes modularization allowing you to only import what you need:

```javascript
const { format } = require('date-fns');
const formattedDate = format(new Date(), 'yyyy-MM-dd');
console.log(formattedDate); // Example output: 2023-04-14
```

Each approach offers different levels of convenience and flexibility for working with dates in JavaScript, from the built-in `Date` object to more sophisticated formatting and manipulation capabilities available through libraries.
