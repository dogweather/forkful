---
title:                "Comparing two dates"
html_title:           "Elm recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Compare Two Dates in Javascript: A Straightforward Guide 

## What & Why?

Comparing two dates determines which date is earlier or later. This comparison is commonly used in scheduling apps, event-driven programming, or anytime you sort items by date.

## How to:

Here's two dates comparison in simple terms:
```Javascript
let date1 = new Date('2022-01-01');
let date2 = new Date('2023-01-01');
console.log(date1 > date2);  // Outputs: false
```
We compare Jan 1, 2022 & Jan 1, 2023 directly. Outputs `false` because date1 (2022) is not greater than date2 (2023).

Ready for another example? Here it is:

```Javascript
let date1 = new Date('2022-01-01');
let date2 = new Date('2021-01-01');
console.log(date1.getTime() === date2.getTime()); // Outputs: false
```
We check if `date1` (2022) is exactly the same day as `date2` (2021). It's `false` because they’re clearly different dates.

## Deep Dive

Historically, JavaScript didn't allow date comparisons using simple operators. You had to extract the time using the `.getTime` method and compare those. These days, JavaScript engines allow direct comparison.

Alternative solutions depend on what you’re trying to achieve. For a date range check, you might use:

```Javascript
let startDate = new Date('2022-01-01');
let endDate = new Date('2022-12-31');
let checkDate = new Date('2022-06-15');

console.log((checkDate >= startDate) && (checkDate <= endDate)); // Outputs: true
```

The above example checks if `checkDate` falls within a specified range.

Lastly, remember that JavaScript compares dates based on the UTC timestamp, which does not consider time zones. So, be careful when handling dates across different time zones.

## See Also

- Mozilla has an excellent web docs, here's a [comprehensive guide](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date) on the Date object.
- For complex date manipulations, check [Moment.js](https://momentjs.com/)