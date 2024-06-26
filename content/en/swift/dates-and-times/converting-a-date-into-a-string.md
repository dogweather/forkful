---
date: 2024-01-20 17:37:39.271868-07:00
description: 'How to: Swift uses `DateFormatter` to turn `Date` objects into readable
  strings. Here''s how.'
lastmod: '2024-03-13T22:45:00.406470-06:00'
model: gpt-4-1106-preview
summary: Swift uses `DateFormatter` to turn `Date` objects into readable strings.
title: Converting a date into a string
weight: 28
---

## How to:
Swift uses `DateFormatter` to turn `Date` objects into readable strings. Here's how:

```Swift
import Foundation

let date = Date()
let formatter = DateFormatter()
formatter.dateFormat = "yyyy-MM-dd HH:mm:ss"
let dateString = formatter.string(from: date)
print(dateString) // Output: "2023-04-05 14:20:35" (or current date and time)
```

Change the `dateFormat` to tweak how your date looks:

```Swift
formatter.dateFormat = "EEEE, MMM d, yyyy"
print(formatter.string(from: date)) // Output: "Wednesday, Apr 5, 2023"
```

## Deep Dive
Before `DateFormatter`, Objective-C and early Swift used `NSDateFormatter`, which is essentially the same thing rebranded. The key is knowing ISO 8601, a common date format standard. Developers must balance custom formats with user locale settings. Why? Dates read differently worldwide. For example, Americans use "MM/dd/yyyy", while many European countries use "dd/MM/yyyy".

Alternatives? Sure. Swift offers `ISO8601DateFormatter` for ISO 8601 dates, and `DateComponentsFormatter` for duration strings, like "42 minutes". You could also go custom with `.formatted()` in Swift 5.5 onwards:

```Swift
let formattedDate = date.formatted(.dateTime.year().month().day().hour().minute().second())
print(formattedDate) // Output will depend on your locale settings
```

Beware: Custom string creation can lead to localization headaches and error-prone code. Stick with formatters and standards when possible.

## See Also
- [Date Formatting](https://developer.apple.com/documentation/foundation/dateformatter) - Apple's Documentation on DateFormatter
