---
date: 2024-01-20 17:33:55.434949-07:00
description: "How to: Back in the day, dates were a haystack of formats and mangled\
  \ calculations. With JavaScript (and TypeScript by extension), the `Date` object\u2026"
lastmod: '2024-04-05T22:50:48.415760-06:00'
model: gpt-4-1106-preview
summary: Back in the day, dates were a haystack of formats and mangled calculations.
title: Comparing two dates
weight: 27
---

## How to:
Let's compare some dates:

```TypeScript
const date1 = new Date('2023-04-01T00:00:00Z');
const date2 = new Date('2023-04-02T00:00:00Z');

// Is date1 before date2?
console.log(date1 < date2); // true

// Is date1 the same as date2?
console.log(date1.getTime() === date2.getTime()); // false

// How many days apart?
const diffTime = Math.abs(date2.getTime() - date1.getTime());
const diffDays = Math.ceil(diffTime / (1000 * 60 * 60 * 24)); 
console.log(diffDays); // 1
```

Sample output:

```
true
false
1
```

## Deep Dive
Back in the day, dates were a haystack of formats and mangled calculations. With JavaScript (and TypeScript by extension), the `Date` object simplified things, standardizing how we handle time.

Alternatives? Sure. Libraries like `moment.js` or `date-fns` augment date handling with extra functionality. But for basic comparisons? Native Date's simplicity often does the job.

Under the hood, `Date.getTime()` gets the milliseconds since epoch (Jan 1, 1970). Comparing these values clears out quirks of time zones and leap seconds, boiling it down to numbers.

## See Also
- [Mozilla Developer Network Date Reference](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date) for the ins and outs of Date objects.
- [You Don't Need Moment.js](https://github.com/you-dont-need/You-Dont-Need-Momentjs) for times you might, or might not, want a library.
- [TypeScript Official Documentation](https://www.typescriptlang.org/docs/) for more on TypeScript's power and pitfalls.
