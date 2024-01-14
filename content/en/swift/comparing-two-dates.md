---
title:    "Swift recipe: Comparing two dates"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why
In Swift programming, comparing two dates can be a useful skill to have in your repertoire. Whether you're building a date picker for your app or calculating the time difference between two events, understanding how to compare dates can save you time and headache.

## How To
To compare two dates in Swift, you first need to create two date objects using the `Date()` initializer. Then, you can use the `compare()` method to compare the two dates, which will return a `ComparisonResult` enum. Here's an example of how to compare two dates:

```Swift
let date1 = Date()
let date2 = Date(timeIntervalSinceNow: 3600) // Adds 1 hour to current date

if date1.compare(date2) == .orderedAscending { // Checks if date1 is before date2
    print("Date 1 is before Date 2")
} else if date1.compare(date2) == .orderedDescending { // Checks if date1 is after date2
    print("Date 1 is after Date 2")
} else { // Dates are equal
    print("Date 1 is equal to Date 2")
}
```

The above code will output "Date 1 is before Date 2" since we added an hour to `date2`.

## Deep Dive
When comparing two dates, it's important to understand the different values that can be returned by the `compare()` method. The `ComparisonResult` enum has three cases: `.orderedAscending`, `.orderedDescending`, and `.orderedSame`. These cases indicate whether the first date is before, after, or equal to the second date, respectively.

Additionally, when comparing dates, it's important to note that they are based on the UTC (Coordinated Universal Time) time zone. So if you want to compare dates based on a specific time zone, you will need to use `Calendar` to convert the dates first. This can be done using the `calendar` property and `dateComponents(in:)` method of `Calendar`.

## See Also
- [Apple Developer Documentation on Comparing Dates](https://developer.apple.com/documentation/foundation/date/comparing_dates)
- [Swift Date and Time by Hacking with Swift](https://www.hackingwithswift.com/articles/117/swift-date-and-time-cheat-sheet)

***Note: This article is written for English readers. For the original version, please visit [link to original article].***