---
date: 2024-01-20 17:33:12.273788-07:00
description: "Comparing two dates means checking if they are the same or determining\
  \ which comes before or after. Programmers often need this for deadlines, event\u2026"
lastmod: '2024-03-11T00:14:34.317820-06:00'
model: gpt-4-1106-preview
summary: "Comparing two dates means checking if they are the same or determining which\
  \ comes before or after. Programmers often need this for deadlines, event\u2026"
title: Comparing two dates
---

{{< edit_this_page >}}

## What & Why?

Comparing two dates means checking if they are the same or determining which comes before or after. Programmers often need this for deadlines, event scheduling, or just tracking time.

## How to:

JavaScript's `Date` objects come in handy. When you compare them, they convert into milliseconds since January 1, 1970, UTC.

```javascript
let date1 = new Date('2021-07-24');
let date2 = new Date('2021-07-25');

console.log(date1 < date2); // true
console.log(date1 > date2); // false
console.log(date1.getTime() === date2.getTime()); // false
```

Sample output:

```
true
false
false
```

## Deep Dive

Under the hood, `Date` objects are just milliseconds. Historically, programmers had to manage date operations manually, calculating the time elapsed from a datum-point, often risking errors. Comparing `Date` objects makes life easier, though yet not error-proof, especially with time zones and daylight saving time.

Alternatives? Sure. Libraries like `moment.js` or `date-fns` help handle complex scenarios and provide additional conveniences for date manipulation.

Implementation wise, it's key to remember that directly comparing `Date` objects (with `==`) compares references, not values. Use `getTime()` for an accurate value comparison. And watch out for time zones when parsing dates; it's easy to get tripped up if you're not careful.

## See Also

- MDN web docs on Date: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date
- Moment.js Library: https://momentjs.com/
- date-fns Library: https://date-fns.org/
