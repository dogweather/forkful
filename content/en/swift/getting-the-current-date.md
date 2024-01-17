---
title:                "Getting the current date"
html_title:           "Swift recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
Getting the current date is a common task in programming that involves retrieving the current date and time from a device or server. It is important for various applications such as scheduling, data tracking, and creating time-sensitive features. Programmers often use this data to ensure accuracy and consistency in their code.

## How to:
To get the current date in Swift, we can use the Date class and its corresponding static methods. Here's a simple example:

```Swift
let currentDate = Date()
```

This creates a new Date instance with the current date and time. You can also get the current date in a specific time zone by using the Date's initializer with a time zone parameter:

```Swift
let currentDate = Date(in: TimeZone.current)
```

To print the current date, we can use a DateFormatter to format it into a readable string:

```Swift
let formatter = DateFormatter()
formatter.dateFormat = "MM/dd/yyyy"
let currentDateString = formatter.string(from: currentDate)
print(currentDateString) // Output: 01/01/2022
```

## Deep Dive:
In the past, getting the current date required complex calculations and conversions. However, with the introduction of the Date class in Swift, this task became much easier. Before Swift 3, developers used the NSDate class to handle dates.

There are also alternative ways to get the current date in Swift, such as using the Calendar class or the NSDateComponents class. However, these are usually used for more specific purposes, and the Date class remains the most straightforward and widely used method.

Under the hood, the Date class is a thin wrapper over a Double value representing the time interval since January 1, 2001, at 00:00:00 UTC. This is known as the Unix epoch, a standard system for representing dates and times in computing.

## See Also:
- Apple's official documentation on the Date class: https://developer.apple.com/documentation/foundation/date
- Tutorial on Date and Time in Swift: https://www.ralfebert.de/ios-examples/date-pickers/ios-date-and-time-pickers/
- Understanding Epoch Time: https://www.epochconverter.com/