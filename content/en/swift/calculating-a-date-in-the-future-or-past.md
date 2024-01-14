---
title:    "Swift recipe: Calculating a date in the future or past"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Why
Calculating dates in the future or past is an essential part of many applications, especially those related to scheduling or time-sensitive tasks. Being able to accurately determine a future or past date can greatly improve the functionality and user experience of an app.

## How To
To calculate a date in the future or past, we can use the `Calendar` and `DateComponents` classes in Swift. The `Calendar` class allows us to specify a specific calendar, such as the Gregorian calendar, while the `DateComponents` class helps us to break down a date into its individual components, such as year, month, day, etc.

```Swift
// Calculate a date 1 year from now
let calendar = Calendar.current
let today = Date()
let oneYearFromNow = calendar.date(byAdding: .year, value: 1, to: today)

// Calculate a date 2 months and 15 days from now
let twoMonthsAndFifteenDaysFromNow = calendar.date(byAdding: DateComponents(month: 2, day: 15), to: today)

// Calculate a date 1 week and 3 days in the past
let oneWeekAndThreeDaysAgo = calendar.date(byAdding: DateComponents(day: -10), to: today) // Note: we use negative values for the `to` parameter to calculate a date in the past
```

The `date(byAdding:to:)` method of the `Calendar` class allows us to add or subtract a certain time interval to/from a date, while the `DateComponents` class allows us to specify the specific components we want to add or subtract.

## Deep Dive
In the `date(byAdding:to:)` method, we use the `Calendar.Component` enum to specify the type of time interval (year, month, day, etc.) we want to add or subtract. We can also use multiple components in a single call, as seen in the second and third example above.

When calculating a date in the future, it is important to consider leap years and the varying lengths of months. The `Calendar` class takes these factors into account when calculating the new date.

## See Also
- [Apple Developer Documentation - Date Components](https://developer.apple.com/documentation/foundation/datecomponents)
- [Apple Developer Documentation - Calendar](https://developer.apple.com/documentation/foundation/calendar)
- [Swift by Sundell - Working with dates in Swift](https://www.swiftbysundell.com/basics/date)