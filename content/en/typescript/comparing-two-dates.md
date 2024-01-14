---
title:    "TypeScript recipe: Comparing two dates"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why

Date comparisons are a common task in many programming languages, including TypeScript. Being able to compare two dates allows developers to check for date ranges, sort events, and perform other operations that require knowledge of when one date falls in relation to another. In this blog post, we'll explore how to compare two dates in TypeScript and the nuances of date comparisons.

## How To

Let's start with a basic example where we have two dates and want to compare them to see which is greater:

```TypeScript
const date1 = new Date(2021, 5, 1);
const date2 = new Date(2021, 5, 10);

if (date1 > date2) {
  console.log("Date1 is greater than Date2");
} else if (date2 > date1) {
  console.log("Date2 is greater than Date1");
} else {
  console.log("Both dates are equal");
}
```

In this example, we create two `Date` objects with different values and use the greater than (`>`) operator to compare them. We can also use other operators such as less than (`<`) and equal (`===`) to compare dates.

It's important to note that `Date` objects in JavaScript and TypeScript are based on the Unix timestamp, which represents the number of milliseconds that have passed since January 1, 1970. This means that when we compare two `Date` objects, we are actually comparing their underlying timestamps. This can sometimes lead to unexpected results, as we'll see in the next section.

Let's now look at how to compare dates with time values included:

```TypeScript
const date1 = new Date(2021, 5, 1, 10, 30, 0);
const date2 = new Date(2021, 5, 1, 9, 0, 0);

if (date1 > date2) {
  console.log("Date1 is later than Date2");
} else if (date2 > date1) {
  console.log("Date2 is earlier than Date1");
} else {
  console.log("Both dates are equal");
}
```

In this example, we've added time values to our `Date` objects. Here, we can see that the comparison is not just based on the date but also on the time. In this case, `Date1` is later than `Date2` because it has a later time value of 10:30 compared to 9:00.

Sometimes, we want to compare dates without taking the time into account. For this, we can use the `Date`'s `toDateString()` method, which returns a string representation of the date without the time component. Here's an example:

```TypeScript
const date1 = new Date(2021, 5, 1, 10, 30, 0);
const date2 = new Date(2021, 5, 1, 9, 0, 0);

if (date1.toDateString() > date2.toDateString()) {
  console.log("Date1 is later than Date2");
} else if (date2.toDateString() > date1.toDateString()) {
  console.log("Date2 is earlier than Date1");
} else {
  console.log("Both dates are equal");
}
```

Here, we're comparing the string representations of the dates and ignoring the time values. This allows us to compare dates at a higher level, without getting into the details of the time component.

## Deep Dive

Although comparing dates may seem straightforward, there are some edge cases and nuances that developers should be aware of. For instance, when comparing `Date` objects, we're actually comparing their underlying timestamps. However, two `Date` objects with the same timestamp will not necessarily be equal, as they can contain different time zone information. This can lead to inconsistencies when comparing dates across time zones.

Another consideration is that `Date` objects can contain invalid values, such as February 30th or 31st. In these cases, the `Date` object will roll over to the next valid date. This can cause unexpected results when comparing dates, so it's important to handle these cases appropriately.

When comparing dates in TypeScript, it's also important to be aware of the internal representation of the `Date` object. This involves taking into account the difference in precision between dates with and without time values.

To learn more about these and other details of date comparisons in TypeScript, check out the official documentation on [Dates, Times, and Time Zones](https://www.typescriptlang.org/docs/handbook/dates.html).

## See Also

Here are some other resources and articles related to date comparisons in TypeScript:

- [Moment.js](https://momentjs.com/) - A popular JavaScript library for date manipulation and formatting.
- [Compar