---
date: 2024-01-20 17:33:55.410107-07:00
description: "\"\u0429\u043E \u0456 \u0427\u043E\u043C\u0443?\" Comparing two dates\
  \ helps us figure out their order and how far apart they are. Programmers do it\
  \ to manage events, reminders, or anything\u2026"
lastmod: '2024-03-11T00:14:23.751909-06:00'
model: gpt-4-1106-preview
summary: "\"\u0429\u043E \u0456 \u0427\u043E\u043C\u0443?\" Comparing two dates helps\
  \ us figure out their order and how far apart they are. Programmers do it to manage\
  \ events, reminders, or anything\u2026"
title: "\u041F\u043E\u0440\u0456\u0432\u043D\u044F\u043D\u043D\u044F \u0434\u0432\u043E\
  \u0445 \u0434\u0430\u0442"
---

{{< edit_this_page >}}

## What & Why?
"Що і Чому?"

Comparing two dates helps us figure out their order and how far apart they are. Programmers do it to manage events, reminders, or anything related to time-dependent features.

## How to:
"Як це зробити:"

Swift's `Date` objects can be compared using comparison operators. Here's how to do it:

```Swift
import Foundation

let now = Date()
let tomorrow = Calendar.current.date(byAdding: .day, value: 1, to: now)!

// Check if one date comes before the other
if now < tomorrow {
    print("Now is earlier than tomorrow.")
} else {
    print("Now is not earlier than tomorrow.")
}

// Check if two dates are the same moment
if now == tomorrow {
    print("Now is the same as tomorrow.")
} else {
    print("Now is not the same as tomorrow.")
}

// Comparing dates for sorting
let dates = [tomorrow, now]
let sortedDates = dates.sorted(by: { $0 < $1 })
print("Sorted dates: \(sortedDates)")
```

Sample output:

```
Now is earlier than tomorrow.
Now is not the same as tomorrow.
Sorted dates: [current date's timestamp, tomorrow's date's timestamp]
```

## Deep Dive
"Детальний огляд"

In the old times of Objective-C, we compared dates using `NSDate` and methods like `compare:`. Now, with Swift, comparison operators make the code cleaner. 

Alternatives to simple comparison include using `Calendar` and `DateComponents` for more complex operations, like comparing only specific components (just the day, month, or year). 

For implementation, Swift uses `TimeInterval` (a `Double` representing seconds) to measure exact differences between two `Date` objects.

## See Also
"Дивіться також"

- Official Swift Documentation: [Date - Apple Developer](https://developer.apple.com/documentation/foundation/date)
- Swift's `Calendar` and `DateComponents`: [Calendar - Apple Developer](https://developer.apple.com/documentation/foundation/calendar)
- A more comprehensive guide to dates in Swift: [Working with Dates in Swift - NSHipster](https://nshipster.com/datecomponents/)
