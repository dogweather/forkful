---
title:                "Converting a date into a string"
date:                  2024-01-20T17:37:38.018725-07:00
model:                 gpt-4-1106-preview
simple_title:         "Converting a date into a string"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Converting a date to a string changes the date object into a text format. Programmers do this for readability, storage, or to display dates to users.

## How to:

```TypeScript
// Simple conversion using toLocaleString()
let date = new Date();
let dateString = date.toLocaleString();
console.log(dateString); // "4/3/2023, 1:15:30 PM" (will vary based on locale)

// ISO format using toISOString()
let isoString = date.toISOString();
console.log(isoString); // "2023-04-03T13:15:30.000Z"

// Custom format using toLocaleDateString()
let customString = date.toLocaleDateString('en-US', {
  year: 'numeric',
  month: 'long',
  day: 'numeric',
});
console.log(customString); // "April 3, 2023"
```

## Deep Dive

Think of a date's string format as its passport, allowing it to travel across system boundaries - from databases to web pages. Historically, we've struggled with inconsistent date formats, which is why standards like ISO 8601 were introduced. This simplifies date exchange worldwide.

Alternatives to built-in methods? Libraries! Moment.js was the go-to for years, but these days date-fns or Luxon are the preferred choices - they're lighter and more modular.

The essence of these conversions lies in the methods used. `toLocaleString()` leans on the user's locale, making it perfect for displaying to users. `toISOString()`, however, stays true to the ISO 8601 format, which is brilliant for serializing and storing dates in a standard format. And `toLocaleDateString()` gives you control over the appearance, catering to specific styling needs.

## See Also

- [Date Object - MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [date-fns documentation](https://date-fns.org/docs/Getting-Started)
- [Luxon documentation](https://moment.github.io/luxon/)
- [ISO 8601 Date and time format](https://www.iso.org/iso-8601-date-and-time-format.html)
