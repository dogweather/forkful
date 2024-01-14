---
title:    "Swift recipe: Calculating a date in the future or past"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why
Calculating dates in the past or future is a common task in many programming projects. It allows us to manipulate and display date and time information in a precise and efficient manner. In this blog post, we will explore how to calculate dates using Swift programming language.

## How To
In Swift, we can calculate dates using the `Date` and `Calendar` classes. Let's take a look at a simple example of calculating a date in the future, three months from today:

```Swift
let currentDate = Date()
let calendar = Calendar.current
let futureDate = calendar.date(byAdding: .month, value: 3, to: currentDate)

print(futureDate) // Output: 2021-03-17 06:25:11 +0000
```

In this example, we first obtain the current date using the `Date` class. Then, we use the `Calendar` class to add three months to the current date, resulting in our desired future date.

We can also calculate dates in the past by using a negative value for the `value` parameter. For example, if we want to find the date six months ago, we can do:

```Swift
let pastDate = calendar.date(byAdding: .month, value: -6, to: currentDate)

print(pastDate) // Output: 2020-09-17 06:25:11 +0000
```

Moreover, we can also specify different components to add or subtract from the date, such as days, years, hours, minutes, and so on. The `Calendar` class also provides us with various options for handling different calendar systems, time zones, and locales.

## Deep Dive
Now that we've seen a basic example of calculating dates in Swift, let's take a deeper look at how it works. In Swift, dates are represented as a single point in time, with a precision of milliseconds. This point in time is calculated based on the number of seconds from a specific reference date, which is January 1, 2001 at 00:00:00 UTC.

The `Calendar` class is responsible for converting this point in time into a specific date and time representation, based on the specified calendar system, time zone, and locale. It also handles any adjustments that need to be made for daylight saving time or leap years.

Furthermore, the `Date` class also provides us with various methods for comparing and manipulating dates, such as checking if one date is before, after, or equal to another date.

## See Also
If you want to learn more about working with dates in Swift, here are some helpful resources:

- [Apple's Documentation on Dates and Times](https://developer.apple.com/documentation/foundation/dates_and_times)
- [Swift Date and Time Tutorial](https://www.ralfebert.de/ios/articles/dateformatters_and_swift/)

Now that you have a better understanding of calculating dates in Swift, you can start using this knowledge in your own projects. Happy coding!