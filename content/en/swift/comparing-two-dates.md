---
title:    "Swift recipe: Comparing two dates"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Why

Comparing dates is a common task in programming, especially when working with data that contains timestamps. Being able to compare two dates allows us to check for any changes or differences and make decisions based on that information. In this blog post, we will explore how to compare two dates in Swift.

## How To

To compare two dates in Swift, we can use the `compare` method from the `Calendar` class. Let's take a look at an example:

```Swift
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "MM/dd/yyyy"
let date1 = dateFormatter.date(from: "05/20/2021")!
let date2 = dateFormatter.date(from: "05/25/2021")!

let comparisonResult = Calendar.current.compare(date1, to: date2, toGranularity: .day)

switch comparisonResult {
    case .orderedSame:
        print("The dates are the same.")
    case .orderedAscending:
        print("date1 comes before date2.")
    case .orderedDescending:
        print("date1 comes after date2.")
}
```

In this example, we first create a `DateFormatter` to convert our date strings into `Date` objects. Then, we use the `compare` method to compare `date1` and `date2` with the `.day` granularity, which means we are only checking for differences in the day component of the dates. The `compare` method returns a `ComparisonResult` with three possible values: `orderedSame` if the dates are the same, `orderedAscending` if the first date comes before the second, and `orderedDescending` if the first date comes after the second.

In the example, the output will be "date1 comes before date2." You can change the granularity to compare the dates with different levels of precision, such as `.month` or `.year`.

## Deep Dive

When comparing dates, it's important to keep time zones in mind. By default, the `Calendar` class uses the current time zone, but you can specify a different time zone if necessary.

You can also compare dates based on different calendar systems, such as the Gregorian, Islamic, or Chinese calendar. This is done by specifying the `calendar` parameter in the `compare` method.

It's also possible to compare dates with different components, such as seconds, minutes, or hours, by using the `compare(_:to:toGranularity:options:)` method instead of the `compare(_:to:toGranularity:)` method.

## See Also

- [Working with Dates and Times in Swift](https://www.swiftbysundell.com/articles/working-with-dates-in-swift/)
- [Date and Time Programming Guide](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/DatesAndTimes/DatesAndTimes.html#//apple_ref/doc/uid/10000039i)
- [NSCalendar Documentation](https://developer.apple.com/documentation/foundation/nscalendar)