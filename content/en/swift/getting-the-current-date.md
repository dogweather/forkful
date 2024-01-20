---
title:                "Getting the current date"
date:                  2024-01-20T15:16:28.225959-07:00
html_title:           "Arduino recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
In Swift, getting the current date involves accessing the system's current time and date settings. Programmers do this to timestamp events, schedule tasks, or just display the date and time in their apps.

## How to:
Grabbing the current date and time in Swift is straightforward:

```Swift
import Foundation

let currentDate = Date()
print(currentDate)
```
Sample Output:
```
2023-04-10 16:20:32 +0000
```
If you want a specific format:

```Swift
import Foundation

let formatter = DateFormatter()
formatter.dateFormat = "yyyy-MM-dd HH:mm:ss"
let formattedDate = formatter.string(from: Date())
print(formattedDate)
```
Sample Output:
```
2023-04-10 16:20:32
```

## Deep Dive
The `Date` struct in Swift is part of the Foundation framework, which came from Objective-C's `NSDate`. Over time, Swift provided a modern approach with `Date` that's more expressive and safer.

There are alternatives to `Date()` for getting the current time. For instance, `NSDate()`, which is roughly the same but less Swift-friendly, and lower-level APIs like `gettimeofday()` for getting more precise system time. But, `Date()` is the go-to for most Swift developers because it balances ease of use with sufficient precision for typical use cases.

`Date()` in Swift gets system time, which is often in Coordinated Universal Time (UTC). So when you print it directly without a format, it appears with a UTC offset. That's why formatters are popular; they adjust the date and time to any specified timezone and format, rendering it human-friendly on display. Implementing your own timezone adjustments without formatters is possible but reinventing the wheel and prone to errors due to daylight saving changes and leap seconds.

## See Also
- Apple's official `Date` documentation: [Date - Apple Developer Documentation](https://developer.apple.com/documentation/foundation/date)
- DateFormatter guide: [DateFormatter - Apple Developer Documentation](https://developer.apple.com/documentation/foundation/dateformatter)
- For deeper insights into date and time in computer systems, check [Computerphile’s video on YouTube](https://www.youtube.com/watch?v=-5wpm-gesOY).