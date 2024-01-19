---
title:                "Comparing two dates"
html_title:           "Elm recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why?

Comparing two dates in programming is the process of determining which date is earlier, later, or whether they are identical. Programmers often do this to perform tasks such as sorting events by date, checking validity of input, or implementing a scheduler.

## How to:

In Swift, there is an in-built system for date comparison through `Date` types and using operators like `==`, `!=`, `<`, `>`, `<=`, `>=`. Here's how you do it:

```Swift
let date1 = Date()
let date2 = Date().addingTimeInterval(3600) // One hour in the future

if date1 < date2 {
    print("date1 comes before date2")
} else if date1 > date2 {
    print("date1 comes after date2")
} else {
    print("The dates are identical")
}
```

For comparing only the date, not including the time, you need to use a `Calendar`:

```Swift
let calendar = Calendar.current
let date1 = calendar.startOfDay(for: Date()) // Today's date
let date2 = calendar.startOfDay(for: Date().addingTimeInterval(86400)) // Tomorrow's date

if date1 < date2 {
    print("date1 comes before date2")
} else if date1 > date2 {
    print("date1 comes after date2")
} else {
    print("The dates are identical")
}
```

## Deep Dive

Historically, date comparison was quite complex, requiring conversion to some common format and ensuring edge cases like leap years were handled correctly. Swift removes much of this complexity by providing intuitive mechanisms.

Although the method shown above is the most straightforward, there are alternatives. For instance, you could use `compare(_:to:granularity:)` method of the `Calendar` object, which compares dates to a specified granularity.

Implementation details come into play when considering time zones. In Swift, `Date` objects are point-in-time, independent of any particular calendar or time zone. Therefore, when comparing dates that were created in a specific calendar or time zone, developers must take care to convert them properly.

## See Also

For more info about Swift's Calendar, check out:
[Apple's Official Documentation on Calendar](https://developer.apple.com/documentation/foundation/calendar)

To dive deep into Swift's Date:
[Apple's Official Documentation on Date](https://developer.apple.com/documentation/foundation/date)

And if you want to explore more about Date and Time Programming Guide:
[Apple's Documentation on Dates and Times](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/DatesAndTimes/DatesAndTimes.html)