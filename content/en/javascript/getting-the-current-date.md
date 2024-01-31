---
title:                "Getting the current date"
date:                  2024-01-20T15:14:56.974399-07:00
html_title:           "Arduino recipe: Getting the current date"
simple_title:         "Getting the current date"

category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
Getting the current date in JavaScript is grabbing the present day's date and time. Programmers do this for things like timestamps, schedules, and time-based logic.

## How to:
```javascript
const now = new Date();
console.log(now.toString());  // Example output: Wed Apr 05 2023 20:46:28 GMT-0400 (Eastern Daylight Time)

console.log(now.toISOString());  // Example output: 2023-04-05T20:46:28.000Z
```

## Deep Dive
Way back, JavaScript's `Date` object was built to handle dates and times. A `Date` object represents a single moment in time, down to the millisecond.

**Alternatives:**
- Libraries like Moment.js (though it's now considered legacy), date-fns, or Luxon can offer more features.
- With Node.js, you can use built-in modules for time, but in most cases, the native `Date` object works just fine.

**Implementation Details:**
- `Date` can turn into a string or a specific format using methods like `.toString(), .toISOString()`.
- Timezone quirks are common sore spots. Note, `.toISOString()` returns UTC time.
- JavaScript counts time as milliseconds since the Unix Epoch (January 1, 1970, 00:00:00 UTC). You can get this with `Date.now()`.

## See Also
- [MDN Web Docs on Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [You Don't Need Moment.js](https://you-dont-need.github.io/You-Dont-Need-Momentjs/)
- [Luxon Documentation](https://moment.github.io/luxon/)
