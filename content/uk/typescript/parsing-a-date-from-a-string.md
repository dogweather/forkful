---
title:                "Аналіз дати з рядка"
date:                  2024-01-20T15:39:01.048697-07:00
html_title:           "Arduino: Аналіз дати з рядка"
simple_title:         "Аналіз дати з рядка"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
## Що це таке та Навіщо?

Parsing a date from a string means converting text that represents a date into a `Date` object. Programmers do this to manipulate and use date-related information in their code.

## How to:
## Як це зробити:

```TypeScript
// Simple parsing using the Date constructor
const dateString: string = '2023-04-01T10:00:00Z';
const parsedDate: Date = new Date(dateString);
console.log(parsedDate);  // Outputs: 2023-04-01T10:00:00.000Z

// Using Date.parse (less recommended)
const timeInMs: number = Date.parse('01 Apr 2023 10:00:00 GMT');
const dateFromMs: Date = new Date(timeInMs);
console.log(dateFromMs);  // Outputs: 2023-04-01T10:00:00.000Z

// Using libraries like date-fns or moment.js for more complex parsing
import { parseISO } from 'date-fns';

const complexDateString: string = '1st of April 2023 at 10 o'clock UTC';
// With date-fns, we need a standard format
const standardizedDate: Date = parseISO('2023-04-01T10:00:00Z');
console.log(standardizedDate);  // Outputs: 2023-04-01T10:00:00.000Z
```

## Deep Dive:
## Поглиблений Розбір:

Parsing dates has been tricky historically because of variation in formats and timezones. JavaScript's `Date` has limitations and quirks. It often uses the local timezone which can be problematic.

Alternatives like `Moment.js` or `date-fns` provide powerful functions for parsing, formatting, and manipulating dates, and they handle edge cases more gracefully.

Details: JavaScript `Date` objects are built on milliseconds since the Unix epoch (1 January 1970 UTC). Parsing strings into dates involves calculating this time value based on the input string's format.

## See Also:
## Дивіться також:

- MDN Web Docs on `Date`: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date
- `date-fns` documentation: https://date-fns.org/
- `Moment.js` guide: https://momentjs.com/docs/#/parsing/