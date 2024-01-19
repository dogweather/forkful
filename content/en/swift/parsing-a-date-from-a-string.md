---
title:                "Parsing a date from a string"
html_title:           "C recipe: Parsing a date from a string"
simple_title:         "Parsing a date from a string"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing a date from a string refers to the process of converting a text representation of a date into a formal date object that your Swift program understands and handles. It enables programmers to manipulate, format, and carry out calculations based on that date information. 

## How to:

Straight to it, Swift provides a built-in class called `DateFormatter` to serve this purpose. Let's look into a basic implementation, where we convert a date string to a `Date` object. 

```Swift
import Foundation

let dateStr = "2025-11-30"
let formatter = DateFormatter()
formatter.dateFormat = "yyyy-MM-dd"
if let date = formatter.date(from: dateStr) {
    print(date)
}
```

When executed, the code will produce the following output: 

```Swift
2025-11-30 00:00:00 +0000
```

## Deep Dive

To abstraction, `DateFormatter` has been a part of Foundation Framework since the early Objective-C days, pre-dating Swift. There are alternatives to it, including libraries like `SwiftDate` and `Timepiece`, but `DateFormatter` is robust enough for many use cases and comes bundled with Swift.

The `DateFormatter` object uses the user's current locale to interpret the contents of the string. If you expect dates to arrive in a specific format regardless of the user's locale, don't forget to explicity set `Locale` of the `DateFormatter`. 

```Swift
formatter.locale = Locale(identifier: "en_US_POSIX")
```

A small note about performance, `DateFormatter` is an expensive object to initialize. If you're parsing multiple dates in one go, instantiate it once and reuse it!

## See Also

To learn more about this topic, check out the following sources:

1. [DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter) - Apple's official documentation.
2. [Parsing date and time from a string with Swift](https://www.hackingwithswift.com/example-code/language/how-to-parse-dates-and-times-from-a-string-with-dateformatter) - An article by Hacking with Swift.
3. [how to convert a string to nsdate in swift 4](https://stackoverflow.com/questions/46344963/how-to-convert-a-string-to-nsdate-in-swift-4) - A relevant StackOverflow thread.