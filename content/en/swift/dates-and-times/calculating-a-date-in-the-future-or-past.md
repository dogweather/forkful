---
title:                "Calculating a date in the future or past"
aliases: - /en/swift/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-20T17:32:09.878769-07:00
model:                 gpt-4-1106-preview
simple_title:         "Calculating a date in the future or past"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why?
Ever needed to find a date in the past or future? Programmatically, we often calculate dates for deadlines, reminders, or events. Knowing how takes the guesswork out and lets your app handle time-sensitive tasks accurately.

## How to:
Swift makes date math straightforward with `Calendar` and `DateComponents`. Here's the gist:

```Swift
import Foundation

// Today's date
let today = Date()

// Get the user's current calendar
let currentCalendar = Calendar.current

// Add 2 weeks to today
if let twoWeeksLater = currentCalendar.date(byAdding: .weekOfYear, value: 2, to: today) {
    print("Two weeks from now: \(twoWeeksLater)")
}

// Subtract 30 days from today
if let thirtyDaysBefore = currentCalendar.date(byAdding: .day, value: -30, to: today) {
    print("Thirty days ago: \(thirtyDaysBefore)")
}
```

Output could be like:
```
Two weeks from now: 2023-04-14 10:26:47 +0000
Thirty days ago: 2023-03-15 10:26:47 +0000
```
Remember, the actual output will vary since `Date()` gives you the current date and time.

## Deep Dive
Before Swift, Objective-C and its clunkier syntax reigned. Swift's `Date`, `Calendar`, and `DateComponents` simplify date operations. These objects respect time zones, handle daylight saving changes, and account for the user's calendar settings – factors that were a slog to manage in Objective-C.

Alternatives include third-party libraries like SwiftDate, which can provide even more convenience and functionality. But for most, Swift's built-in tools work just fine.

Dates are complex. They're not just numbers to increase or decrease; they involve calendars, locale specificities, and time zones. Apple's Foundation framework tackles this complexity, making sure your future and past date calculations make sense worldwide.
