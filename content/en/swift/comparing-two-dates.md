---
title:                "Swift recipe: Comparing two dates"
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why

In programming, we often come across the need to compare two dates. This can be for tasks such as sorting data, calculating time differences, or simply checking if one date falls before or after another. Fortunately, Swift makes it easy to compare dates, and in this blog post, we will explore the different ways of doing so.

## How To

To compare two dates in Swift, we can use the `compare` method, which returns a `ComparisonResult` value indicating the relationship between the two dates. Here is an example where we have two `Date` objects representing the current date and a date in the past:

```Swift
let currentDate = Date()
let pastDate = Date().addingTimeInterval(-3600) //1 hour ago
```

We can now use the `compare` method to check if the past date falls before the current date:

```Swift
if pastDate.compare(currentDate) == .orderedAscending {
    print("The past date falls before the current date.")
}
```

The output will be: "The past date falls before the current date."

We can also use the `compare` method to check if the dates are equal or if one is after the other. Here is an example:

```Swift
if pastDate.compare(currentDate) == .orderedSame {
    print("The dates are equal.")
} else if pastDate.compare(currentDate) == .orderedDescending {
    print("The past date falls after the current date.")
}
```

The output will be: "The past date falls after the current date."

## Deep Dive

Behind the scenes, the `compare` method is using the `timeIntervalSinceReferenceDate` property, which represents the number of seconds since January 1st, 2001. The `ComparisonResult` values are simply a way to easily interpret the result of this comparison.

In addition to the `compare` method, we can also use the `==` and `<` operators to compare dates. The `==` operator checks if the dates are equal, while the `<` operator checks if one date is earlier than the other. Here is an example:

```Swift
if pastDate == currentDate {
    print("The dates are equal.")
} else if pastDate < currentDate {
    print("The past date falls before the current date.")
}
```

The output will be: "The past date falls before the current date."

It is important to note that when comparing dates, the time zone and daylight saving time changes are taken into consideration. This means that two dates can have the same point in time, but if they are in different time zones, the comparison may result in different values.

## See Also

- Official Apple documentation on comparing dates in Swift: https://developer.apple.com/documentation/foundation/date/comparing_dates
- Tutorial on working with dates and time in Swift: https://www.hackingwithswift.com/articles/117/a-quick-look-at-how-to-work-with-dates-in-swift