---
title:    "Gleam recipe: Comparing two dates"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why

When working with dates in any programming language, it is important to be able to compare them. This allows us to check for things like past or future dates and to sort a list of dates in a particular order. In this blog post, we will learn how to compare two dates using the Gleam programming language.

## How To

To compare two dates in Gleam, we will use the `Date.compare` function. This function takes in two `Date` values and returns an `Order` type. This `Order` type is used to determine if the first date is less than, equal to, or greater than the second date.

To illustrate this, let's create two dates and use the `Date.compare` function to compare them:

```Gleam
let date1 = Date.new(2021, 5, 1)
let date2 = Date.new(2021, 6, 1)
let order = Date.compare(date1, date2)
```

In this example, `date1` is May 1st, 2021 and `date2` is June 1st, 2021. We then use the `Date.compare` function to compare these two dates. The `order` variable will now hold the `Order` type, which in this case will be `Greater`.

We can also use the `Date.compare` function to check for equality between two dates. If the two dates are equal, the `Order` type will be `Equal`. Let's see an example of this:

```Gleam
let date1 = Date.new(2021, 1, 1)
let date2 = Date.new(2021, 1, 1)
let order = Date.compare(date1, date2)
```

In this case, `date1` and `date2` are both January 1st, 2021 and the `order` variable will hold the `Equal` type.

## Deep Dive

Internally, the `Date.compare` function uses the `Calendar.compare` function. This function takes in two `Data.Calendar.Date` values and compares them, returning either `OrderLess`, `OrderEqual`, or `OrderGreater`.

The `Calendar.compare` function also takes into account the time component of the dates. This means that if two dates have different times, they can still be considered equal if they are on the same day. For example:

```Gleam
let date1 = DateTime.new(2021, 4, 1, Time.new(10, 0, 0))
let date2 = DateTime.new(2021, 4, 1, Time.new(15, 0, 0))
let order = Date.compare(date1, date2)
```

In this example, `date1` and `date2` have the same date, but different times. However, the `order` variable will hold the `Equal` type, since the time component is not taken into account when comparing dates.

## See Also

- Gleam Date Documentation: https://gleam.run/documentation/stdlib/date
- Date.compare function source code: https://github.com/gleam-lang/gleam_stdlib/blob/master/src/date.gleam#L39-L46
- Calendar.compare function source code: https://github.com/gleam-lang/gleam_stdlib/blob/master/src/calendar.gleam#L49-L57