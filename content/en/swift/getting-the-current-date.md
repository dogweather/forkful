---
title:                "Swift recipe: Getting the current date"
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## WhyGetting the current date may not seem like the most exciting programming task, but it is a crucial aspect of many applications. Whether you need to display the date to the user, or use it for data tracking purposes, having the ability to accurately retrieve the current date is essential. ## How ToTo get the current date in Swift, we can use the built-in `Date()` function. First, we need to import the `Foundation` framework, which contains the `Date` class. Then, we can simply create a new instance of `Date` and use its `now` property to get the current date and time. Here's an example:

```Swift
import Foundation

let currentDate = Date()
print(currentDate)
```

This code will print out the current date and time in the following format:

```Swift
2021-04-01 12:00:00 +0000
```

Note that the `now` property of `Date` returns a `Date` object in UTC time. If you want to display the date and time in a specific time zone, you can use the `Formatter` class to convert it. For example, if you want to display the current date in Eastern Standard Time, you can do the following:

```Swift
let easternFormatter = DateFormatter()
easternFormatter.timeZone = TimeZone(identifier: "EST")
easternFormatter.dateFormat = "yyyy-MM-dd HH:mm:ss"

print(easternFormatter.string(from: currentDate))
```

This will print out the current date and time in Eastern Standard Time:

```Swift
2021-04-01 07:00:00
```

## Deep Dive

Behind the scenes, the `Date` class is simply a wrapper for an underlying `NSTimeInterval` value, which represents the number of seconds since January 1, 2001. The current date and time can be calculated by adding this value to the reference date of January 1, 2001. This may seem relatively simple, but there are many factors that go into accurately retrieving the current date and time, such as accounting for leap years and time zones.

Additionally, the `Date` class also contains methods and properties for comparing dates, getting specific components (such as year, month, day, etc.), and performing arithmetic operations on dates.

## See Also

- [Apple Developer Documentation: Date](https://developer.apple.com/documentation/foundation/date)
- [raywenderlich.com tutorial: Working with Dates in Swift](https://www.raywenderlich.com/7673532-swift-date-cheat-sheet)