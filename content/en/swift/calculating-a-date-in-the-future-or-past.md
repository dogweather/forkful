---
title:                "Calculating a date in the future or past"
html_title:           "Swift recipe: Calculating a date in the future or past"
simple_title:         "Calculating a date in the future or past"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why?

Calculating dates in the future or past is a common task for programmers, as it allows them to manipulate and work with dates in their code. This can include things like setting due dates for tasks, scheduling reminders, or creating event calendars. By being able to calculate dates in the future or past, programmers can create more dynamic and functional applications.

## How to:

To calculate a date in the future or past, we can use the `date(byAdding:to:value:wrappingComponents:)` method within the `Calendar` class in Swift. This method takes in three parameters: a `DateComponents` object, an `Int` value, and a `Date` object. The `DateComponents` object specifies the unit of time we want to add or subtract (e.g. days or hours), the `Int` value represents the number of units we want to add or subtract, and the `Date` object is the starting date from which we want to calculate.

To get the date 7 days from today, we would use the following code:

```Swift
let today = Date()
let calendar = Calendar.current
let futureDate = calendar.date(byAdding: .day, value: 7, to: today)
```

We can also calculate a date in the past by simply using a negative value in the `value` parameter. For example, to get the date 2 months ago, we would use `value: -2`. The `date(byAdding:to:value:wrappingComponents:)` method also takes into consideration different calendar systems and leap years, so we can be confident that our calculations will be accurate.

## Deep Dive:

Historically, calculating dates in the future or past has been a complex task due to the different calendar systems and leap years used around the world. However, with advancements in programming languages, this task has become much easier. Some alternative methods for calculating dates in the future or past include using the `date(byAdding:to:value:wrappingComponents:)` method from the `NSDate` class in Objective-C or using a third-party date library like `Moment.js` for JavaScript.

Under the hood, the `date(byAdding:to:value:wrappingComponents:)` method uses the `DateComponents` class to handle the addition or subtraction of units of time. This class provides a high level of customization and allows us to specify multiple units of time to be added or subtracted at once.

## See Also:

To learn more about calculating dates in the future or past in Swift, check out the official documentation for the `Calendar` class and the `DateComponents` class. You can also explore third-party libraries like `Moment.swift` for additional functionality.