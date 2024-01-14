---
title:                "Swift recipe: Calculating a date in the future or past"
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why

Calculating dates in the future or past is a common task in many programming projects, especially when dealing with events, reminders, or scheduling. Using Swift's built-in date and time functions, we can easily determine the exact date and time we need, making our code more efficient and accurate.

## How To

To calculate a date in the future or past, we first need to create a `Date` object using the `Date()` initializer. This will give us the current date and time.

```Swift
let currentDate = Date()
```

Next, we can use the `Calendar` class to manipulate this date object. This class provides various methods to add or subtract time units such as days, weeks, or even years. For example, to calculate the date 3 days from now, we can use the `date(byAdding:to:wrappingComponents:)` method.

```Swift
let futureDate = Calendar.current.date(byAdding: .day, value: 3, to: currentDate)
print(futureDate) // Output: Optional(2021-04-27 14:52:06 +0000)
```

Similarly, we can also calculate a date in the past by using a negative value for the `value` parameter.

```Swift
let pastDate = Calendar.current.date(byAdding: .month, value: -2, to: currentDate)
print(pastDate) // Output: Optional(2021-01-25 14:52:06 +0000)
```

We can also use the `date(from:components:)` method to specify a specific date and time we want to calculate from. For example, we can use this method to find the date and time exactly one year from now.

```Swift
var dateComponents = DateComponents()
dateComponents.year = 1

if let futureDate = Calendar.current.date(from: dateComponents) {
    print(futureDate) // Output: Optional(2022-04-25 14:52:06 +0000)
}
```

## Deep Dive

Behind the scenes, Swift's `Date` uses the Gregorian calendar system to perform these calculations. This means that it takes into account factors such as leap years, daylight savings time, and time zones. It also takes care of edge cases like going from December to January or handling leap seconds.

Additionally, the `date(byAdding:to:wrappingComponents:)` method has a third parameter `wrappingComponents` which determines what happens when the calculated date falls outside the boundaries of a particular time unit. For example, if we add 4 weeks to April 25th, the wrapping component will determine whether to return the date as May 23rd or move it to the next month.

## See Also

To learn more about working with dates in Swift, check out the following resources:

- [Apple Developer Documentation on Date and Time](https://developer.apple.com/documentation/foundation/date)
- [Ray Wenderlich's tutorial on Working with Dates in Swift](https://www.raywenderlich.com/10212543-dates-and-times-in-swift)
- [Hacking with Swift's advanced tutorial on Date and Time](https://www.hackingwithswift.com/ios-swiftui/working-with-dates)

*Note: Make sure to regularly update your Swift version to ensure that you have access to the latest date and time functions and improvements.