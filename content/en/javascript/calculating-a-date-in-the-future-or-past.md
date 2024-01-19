---
title:                "Calculating a date in the future or past"
html_title:           "Javascript recipe: Calculating a date in the future or past"
simple_title:         "Calculating a date in the future or past"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why?

Calculating a date in the future or past is a common task. We do this to manage events, expiry checks, and reminder systems.

## How to:

Here's the essential code. Adjust `daysToAdd` or use `-` for past dates.
```JavaScript
var daysToAdd = 5;
var date = new Date();

date.setDate(date.getDate() + daysToAdd);

console.log(date);
```
Run the script, and you'll see a date 5 days from today.

## Deep Dive

Historically, `setDate()` and `getDate()` methods arose from Java in the 90s, to simplify date manipulation. There are alternatives, but none native and as straightforward.

You could use libraries like Moment.js or Day.js. Here's an example using Day.js:

```JavaScript
var dayjs = require('dayjs')
var date = dayjs().add(5, 'day').toDate();

console.log(date);
```
While libraries provide additional functions, they add dependency and load.

What's happening in our original code? `getDate()` fetches the day of the month, `setDate()` alters it. So, when we `setDate(date.getDate() + daysToAdd);`, we shift the date.

## See Also

- JavaScript Date setDate() Method: [https://www.w3schools.com/jsref/jsref_setdate.asp](https://www.w3schools.com/jsref/jsref_setdate.asp)
- JavaScript Date getDate() Method: [https://www.w3schools.com/jsref/jsref_getdate.asp](https://www.w3schools.com/jsref/jsref_getdate.asp)
- Day.js Documentation: [https://day.js.org/](https://day.js.org/)
- Moment.js Documentation: [https://momentjs.com/docs/](https://momentjs.com/docs/)