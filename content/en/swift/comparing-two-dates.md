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

## What & Why?
Comparing two dates is a common task in programming where we need to determine whether one date is before, after, or equal to another date. This is important for applications that deal with scheduling, time-sensitive events, or data analysis. By comparing dates, programmers can accurately organize and manipulate data based on time.

## How to:
To compare two dates in Swift, we can use the ```compare()``` method of the ```Date``` class. This method takes in another ```Date``` object as a parameter and returns a ```ComparisonResult``` enum which tells us the relationship between the two dates.

```Swift
// Example 1: Comparing two dates
let date1 = Date() // current date
let date2 = Date(timeIntervalSinceNow: 3600) // date an hour from now

let result = date1.compare(date2)
print(result) // Output: ComparisonResult.orderedAscending
```

The ```ComparisonResult``` enum has three cases: ```orderedAscending```, ```orderedDescending```, and ```orderedSame```. In the example above, we can see that ```date1``` is before ```date2``` since the result is ```orderedAscending```.

We can also use the ```<```, ```>```, and ```==``` operators to compare two dates directly.

```Swift
// Example 2: Using operators to compare dates
let date3 = Date(timeIntervalSince1970: 0) // Unix epoch (January 1, 1970)

let isEarlier = date3 < date1
let isEqual = date1 == date2

print(isEarlier) // Output: true
print(isEqual) // Output: false
```

## Deep Dive
Comparing dates can be a complex task, especially when considering time zones and leap years. In the past, programmers have used libraries like ```NSCalendar``` and ```NSDateComponents``` to compare dates in Swift. However, with the introduction of ```Calendar``` and ```DateComponents``` in Swift 3, these older methods are no longer recommended.

When comparing dates, it is important to consider the precision of the dates. Dates with a higher precision (down to milliseconds) will produce more accurate results than dates with lower precision. Additionally, when comparing dates with a ```TimeInterval``` of a larger magnitude, it is recommended to use the ```timeIntervalSince()``` method instead of ```compare()``` for better performance.

## See Also
- [Apple Developer Documentation - Date](https://developer.apple.com/documentation/foundation/date)
- [Swift Docs - Comparing Dates](https://docs.swift.org/swift-book/LanguageGuide/AdvancedOperators.html#ID43)