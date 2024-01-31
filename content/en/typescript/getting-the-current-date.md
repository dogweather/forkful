---
title:                "Getting the current date"
date:                  2024-01-20T15:16:53.602312-07:00
simple_title:         "Getting the current date"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
Getting the current date in your code means grabbing the present moment down to the day. Programmers do this to timestamp events, handle scheduling, and track duration or intervals.

## How to:
Here's how you snag the current date in TypeScript:

```typescript
// Get the current date and time
const now = new Date();

// Log it to the console
console.log(now);
```

Sample output might look like this:

```
2023-04-01T12:34:56.789Z
```

But if you just want the date without the time:

```typescript
const today = new Date().toISOString().split('T')[0];

console.log(today);
```

And that'll give you:

```
2023-04-01
```

## Deep Dive
JavaScript's `Date` object is what you're working with in TypeScript for dates and times. It's been there since the early days, created as part of ECMAScript 1 in 1997. Alternatives to the native `Date` include libraries like `moment.js` or `date-fns`, which offer more features and better parsing.

Under the hood, `new Date()` gets you the number of milliseconds since the Unix Epoch (January 1, 1970). That's how computers track time. Timezones can be tricky, especially when you need to display dates to users across the globe. By default, `new Date()` will use the system's local time. The `toISOString()` method converts the date to Coordinated Universal Time (UTC) and formats it as an ISO string.

## See Also
- MDN Web Docs on `Date`: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date
- Moment.js: https://momentjs.com/
- Date-fns: https://date-fns.org/
- Timezone handling in JavaScript: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleTimeString
