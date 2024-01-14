---
title:                "Swift recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Why Compare Dates in Swift?

Comparing dates is a common task in programming, especially when dealing with time-sensitive data. In Swift, there are built-in methods and functions that make it easy to compare dates and determine their relative order. In this blog post, we will discuss why you might need to compare dates in Swift and how to do it effectively.

## How To Compare Dates in Swift

To compare two dates in Swift, we can use the `compare()` method provided by the `Date` class. This method takes in another `Date` object as a parameter and returns a `ComparisonResult` enum value. There are three possible values that can be returned:

- `.orderedAscending`: the receiver date is earlier than the parameter date
- `.orderedDescending`: the receiver date is later than the parameter date
- `.orderedSame`: the receiver date is the same as the parameter date

We can use this method to compare two dates and determine their relative order. For example, let's compare two dates representing today and yesterday:

```
let today = Date()
let yesterday = Calendar.current.date(byAdding: .day, value: -1, to: today)!

let comparison = today.compare(yesterday)

switch comparison {
case .orderedAscending:
    print("Today is earlier than yesterday")
case .orderedDescending:
    print("Today is later than yesterday")
case .orderedSame:
    print("Today is the same as yesterday")
}
```

The output of this code would be `Today is later than yesterday`, since the receiver date (today) is indeed later than the parameter date (yesterday). 

## Deep Dive: Understanding Date Components

In the previous example, we used the `Calendar` class to create a `Date` object representing yesterday. This shows us an important aspect of comparing dates in Swift - time precision. When comparing dates, we need to take into account the time components as well as the date components. If two dates are on the same day but at different times, the `compare()` method will return `.orderedSame` even though the times are different. 

To compare dates with more precision, we can use the `Calendar` class to extract specific components from the `Date` objects. For example, we can extract the year, month, and day components using the `dateComponents(_:from:)` method and then compare them individually. This allows us to consider the time components as well and get a more accurate comparison. 

## See Also

- [Apple Developer Documentation on Comparing Dates in Swift](https://developer.apple.com/documentation/foundation/date/comparing_dates)
- [Swift Date and Time Tutorial](https://www.hackingwithswift.com/read/20/overview)
- [Stack Overflow Discussion on Comparing Dates in Swift](https://stackoverflow.com/questions/31675665/how-to-compare-two-dates-in-swift)