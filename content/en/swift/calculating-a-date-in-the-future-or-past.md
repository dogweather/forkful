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

## Why
Calculating dates in the future or past can be incredibly useful in applications that involve scheduling or time-sensitive tasks. For example, a calendar app may need to calculate the date of a future event or a reminder app may need to calculate the date of a past event.

## How To
To calculate a date in the future or past in Swift, we first need to use the `Date` and `Calendar` classes. The `Date` class represents a specific point in time, while the `Calendar` class allows us to manipulate dates and time components.

To calculate a future date, we can use the `date(byAdding:)` method on the `Calendar` class. This method takes in a `DateComponents` object, which specifies how many years, months, days, etc. we want to add to the current date. For example, if we want to calculate the date 5 weeks from today, we can use the following code:

```Swift
let calendar = Calendar.current
let futureDate = calendar.date(byAdding: .weekOfMonth, value: 5, to: Date())
```

To calculate a past date, we can use the `date(byAdding:)` method again, but with a negative value. For example, if we want to calculate the date 3 months ago, we can use the following code:

```Swift
let calendar = Calendar.current
let pastDate = calendar.date(byAdding: .month, value: -3, to: Date())
```

The output for both of these calculations will be a `Date` object representing the calculated date.

## Deep Dive
The `Calendar` class also has other methods that allow for more specific date calculations, such as `date(bySetting: value:of:)` and `date(bySettingHour: minute: second: of:)`. These methods take in a specific date component (such as month, day, hour, etc.) and allow you to set a specific value for that component. You can also use the `dateComponents(_:from:to:)` method to calculate the difference between two dates.

It's important to note that date calculations can be affected by time zone and daylight saving time. To ensure accurate date calculations, make sure to set the `timeZone` property on your `Calendar` instance.

## See Also
- [Apple's documentation on the Date and Calendar classes](https://developer.apple.com/documentation/foundation/date)
- [A guide on date and time calculations in Swift](https://medium.com/@tanmaytuteja/calculating-future-or-past-dates-in-swift-5cc329c9d48f)