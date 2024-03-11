---
date: 2024-01-20 17:31:59.655976-07:00
description: "Calculating a future or past date is about modifying a current date\
  \ to see what day it'll be, say, 10 days from now, or what day it was 10 days ago.\u2026"
lastmod: '2024-03-11T00:14:33.730170-06:00'
model: gpt-4-1106-preview
summary: "Calculating a future or past date is about modifying a current date to see\
  \ what day it'll be, say, 10 days from now, or what day it was 10 days ago.\u2026"
title: Calculating a date in the future or past
---

{{< edit_this_page >}}

## What & Why?

Calculating a future or past date is about modifying a current date to see what day it'll be, say, 10 days from now, or what day it was 10 days ago. Programmers do this for features like expiration dates, scheduling events, or figuring out time differences.

## How to:

```TypeScript
// Get current date
const today: Date = new Date();

// Calculate 10 days in the future
const tenDaysLater: Date = new Date(today.getTime() + (10 * 24 * 60 * 60 * 1000));
console.log(`Ten days from now: ${tenDaysLater.toDateString()}`);

// Calculate 10 days in the past
const tenDaysBefore: Date = new Date(today.getTime() - (10 * 24 * 60 * 60 * 1000));
console.log(`Ten days before was: ${tenDaysBefore.toDateString()}`);
```
Sample Output:
```
Ten days from now: Sun Apr 23 2023
Ten days before was: Wed Apr 03 2023
```

## Deep Dive

Historically, managing dates in JavaScript—and by extension TypeScript—has been tricky due to the quirks of the Date object and timezones. Alternative libraries like Moment.js and date-fns have offered abstractions to handle this complexity. With ES6, better support for internationalization arrived via the `Intl` API, which TypeScript can also use.

When calculating dates, watch for daylight saving changes and leap seconds. These can throw off straightforward calculations like adding 24 hours to a date. Also, always consider the user's locale and timezone when displaying calculated dates.

For broad compatibility and flexibility, you might opt for libraries like `date-fns` or `Luxon`, which are modular and can be great for complex tasks. For example, with `date-fns`, you can add days easily:

```TypeScript
import { addDays } from 'date-fns';

const result = addDays(new Date(2023, 3, 13), 10); // April 13, 2023 + 10 days
console.log(result.toDateString());
```

They also handle edge cases and timezone issues, taking a lot of the pain out of date arithmetic.

## See Also

- [MDN Date Reference](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [date-fns Library](https://date-fns.org/)
- [Luxon Documentation](https://moment.github.io/luxon/#/)
- [TypeScript Official Documentation](https://www.typescriptlang.org/docs/)
