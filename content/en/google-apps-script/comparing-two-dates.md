---
title:                "Comparing two dates"
date:                  2024-02-01T13:43:23.293625-07:00
model:                 gpt-4-0125-preview
simple_title:         "Comparing two dates"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/google-apps-script/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why?

We're diving into how to compare two dates in Google Apps Script, an essential skill for any task needing date validation or events scheduling. Why bother? Because knowing whether a date comes before, after, or exactly coincides with another is crucial in tons of programming scenarios, from booking systems to reminder apps.

## How to:

Comparing dates might sound daunting, but it's really just a few lines of code. Here’s a quick example to show you the ropes:

```javascript
function compareDates() {
  var date1 = new Date('2023-04-01T00:00:00Z');
  var date2 = new Date('2023-04-10T00:00:00Z');
  
  if (date1 < date2) {
    Logger.log('Date1 comes before Date2');
  } else if (date1 > date2) {
    Logger.log('Date1 comes after Date2');
  } else {
    Logger.log('Both dates are the same');
  }
}
```

Sample output when you run this function:
```
Date1 comes before Date2
```

That's it. You can tweak the dates in the example and play around with it to see different outcomes.

## Deep Dive

Google Apps Script, built on top of JavaScript, handles dates much like its parent language. The comparison we did above relies on JavaScript's ability to compare date objects directly, which is straightforward but can sometimes lead you into the tricky territory of time zones and daylight saving times.

Historically, date comparisons in programming have been fraught with these kinds of issues, prompting the development of libraries like Moment.js for more complex operations. While Google Apps Script can use external libraries, for simple comparisons, the native Date object works just fine.

Remember, when working with dates that include times, even a one-second difference means the dates aren't equal. Hence, if you're only interested in comparing dates (ignoring the time part), make sure to normalize your dates to a consistent time, often midnight.

While direct comparison works for many cases, developers often turn to date-fns or dayjs for more nuanced operations, especially if they're dealing with multiple time zones or need to format dates in specific ways. However, for the scope of Google Apps Script and most typical app-scripting tasks, sticking to the native Date object methods is both efficient and sufficient.
