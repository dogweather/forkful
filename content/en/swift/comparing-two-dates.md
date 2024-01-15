---
title:                "Comparing two dates"
html_title:           "Swift recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why

Comparing dates is a common task in programming that allows us to check if certain events have occurred or to determine the amount of time that has passed between two dates. Whether you need to keep track of deadlines, calculate age, or schedule tasks, being able to compare dates is an essential skill for any developer.

## How To

Comparing dates in Swift is a straightforward process that involves using the `Date` and `Calendar` classes. Let's take a look at some code examples to see how it's done:

```Swift
// Creating two date objects
let date1 = Date()
let date2 = Date().addingTimeInterval(60)

// Comparing if date1 is before date2
if date1 < date2 {
    print("date1 is before date2")
}

// Comparing if date1 is after date2
if date1 > date2 {
    print("date1 is after date2")
}
```

Output:
```
date1 is before date2
```

In the above example, we created two `Date` objects using the `Date()` initializer. We also used the `addingTimeInterval()` method to add 60 seconds to `date2` so that it would be one minute ahead of `date1`. Then, we used the comparison operators `<` and `>` to check if one date is before or after the other.

If you're comparing dates with a more specific level of precision, such as comparing only the dates and ignoring the time, you can use the `isDate()` method of the `Calendar` class. Here's an example:

```Swift
// Creating a calendar instance
let calendar = Calendar.current

// Getting the date components of date1
let date1Components = calendar.dateComponents([.day, .month, .year], from: date1)

// Getting the date components of date2
let date2Components = calendar.dateComponents([.day, .month, .year], from: date2)

// Checking if the dates have the same day, month, and year
if calendar.isDate(date1, equalTo: date2, toGranularity: .day) {
    print("date1 and date2 have the same day, month, and year")
}
```

Output:
```
date1 and date2 have the same day, month, and year
```

In the above example, we used the `dateComponents()` method to get specific components from the dates, and then we used the `isDate()` method to compare only the day, month, and year components.

## Deep Dive

Under the hood, Swift's `Date` class is a typealias for the `Foundation` framework's `NSDate` class, which represents a specific point in time. This means that when comparing dates in Swift, you are actually comparing timestamps.

Additionally, the `Calendar` class gives you control over how dates are compared by allowing you to specify a `Calendar.Identifier`, which represents a specific type of calendar (e.g. Gregorian, Islamic, Chinese). This ensures that your comparisons are accurate and reflect the expected cultural and societal norms.

## See Also

- [Apple Developer Documentation: Foundation Framework](https://developer.apple.com/documentation/foundation)
- [NSCalendar - wwdc2016](https://nshipster.com/nscalendar/)