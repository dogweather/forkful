---
date: 2024-02-03 19:02:47.182335-07:00
description: "How to: Swift's `Foundation` framework provides the `Date` class, making\
  \ it straightforward to get the current date and time. Here is a basic example of\u2026"
lastmod: '2024-03-13T22:45:00.405594-06:00'
model: gpt-4-0125-preview
summary: Swift's `Foundation` framework provides the `Date` class, making it straightforward
  to get the current date and time.
title: Getting the current date
weight: 29
---

## How to:
Swift's `Foundation` framework provides the `Date` class, making it straightforward to get the current date and time. Here is a basic example of how to get the current date:

```swift
import Foundation

let currentDate = Date()
print(currentDate)
```

This will output something like:

```
2023-04-12 07:46:23 +0000
```

The output format follows the ISO 8601 standard, using the UTC time zone. However, you might want to format this date for display purposes. Swift's `DateFormatter` class comes to the rescue:

```swift
let formatter = DateFormatter()
formatter.dateStyle = .long
formatter.timeStyle = .medium
let formattedDate = formatter.string(from: currentDate)
print(formattedDate)
```

Sample output could be:

```
April 12, 2023 at 10:46:23 AM
```

Note that the output format will vary depending on the locale of the device running the code.

For projects that require more complex date manipulation, many Swift developers turn to third-party libraries such as `SwiftDate`. Here's how you might use `SwiftDate` to get the current date in a specific time zone and format:

First, add `SwiftDate` to your project using SPM, CocoaPods, or Carthage. Then:

```swift
import SwiftDate

let rome = Region(calendar: .gregorian, zone: .europeRome, locale: .current)
let currentDateInRome = DateInRegion(Date(), region: rome)
print(currentDateInRome.toFormat("yyyy-MM-dd HH:mm:ss"))
```

This could output:

```
2023-04-12 09:46:23
```

Using `SwiftDate`, you can easily manipulate dates and times for different time zones and locales, simplifying complex date handling tasks in your Swift applications.
