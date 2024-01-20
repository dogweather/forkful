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

Calculating a date in the future or past involves manipulating dates by adding or subtracting days, months, or years. Programmers do it for tasks like setting reminders, comparing dates, or handling time-bound records.

## How to:

Let's see the Swift way to do it. Here's how to calculate a date 10 days after the current date.

```Swift
import Foundation
let currentDate = Date()
var dateComponent = DateComponents()

dateComponent.day = 10
let futureDate = Calendar.current.date(byAdding: dateComponent, to: currentDate)

print(futureDate!)
```

And here's how to calculate a date 5 days before the current date.

```Swift
dateComponent.day = -5
let pastDate = Calendar.current.date(byAdding: dateComponent, to: currentDate)

print(pastDate!)
```

In both cases, the `DateComponents` class and the `Calendar` class are used to achieve the task.

## Deep Dive

Historically, calculating dates has been a challenge due to varying lengths of months, leap years, and time zones. With Swift, it’s simplified with utilities like `DateComponents` and `Calendar`.

There are also other techniques, such as converting dates to timestamps (seconds from a fixed point in time), performing the arithmetic, then converting back to a date. However, this method won't account for irregularities like leap years.

The implementation uses the Gregorian calendar. You can set a different calendar if needed. For large additions or subtractions, it even manages transitions between months with different numbers of days.

## See Also

Interested in exploring more about date manipulation in Swift and other areas, check out the following sources:

- [Apple's Date and Time Programming Guide](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/DatesAndTimes/DatesAndTimes.html)
- [Swift’s Calendar and DateComponents documentation](https://developer.apple.com/documentation/foundation/calendar)
- [An in-depth guide to handling dates in Swift](https://www.hackingwithswift.com/read/10/overview)