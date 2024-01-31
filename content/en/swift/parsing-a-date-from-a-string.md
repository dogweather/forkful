---
title:                "Parsing a date from a string"
date:                  2024-01-20T15:38:21.384518-07:00
html_title:           "Arduino recipe: Parsing a date from a string"
simple_title:         "Parsing a date from a string"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing a date from a string means converting the textual representation of a date (like "2023-04-01") into a Date object. Programmers do this to manipulate dates, perform calculations, or display in different formats.

## How to:

Swift makes parsing dates pretty straightforward with `DateFormatter`. Here's a quick example:

```Swift
import Foundation

let dateString = "2023-04-01"
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "yyyy-MM-dd"

if let parsedDate = dateFormatter.date(from: dateString) {
    print("Parsed date: \(parsedDate)")
} else {
    print("Failed to parse date.")
}
```

Sample output might look like this, depending on your timezone:

```
Parsed date: 2023-03-31 22:00:00 +0000
```

Remember, the output is in UTC by default!

## Deep Dive

As early as Objective-C, iOS developers had `NSDateFormatter`, and it carried over to Swift as `DateFormatter`. Historically, handling dates was a big pain due to variations in format and time zones. Thankfully, `DateFormatter` in Swift standardizes this process.

While `DateFormatter` is good for common scenarios, alternatives like the `ISO8601DateFormatter` exist for ISO 8601 formats, and you can even dive into the lower-level `Cocoa` API with `CFDateFormatter` for more control.

In implementing a date parser, always set the `locale` to `posix` (`en_US_POSIX`) to avoid unexpected behavior due to user settings. Also, be mindful of performance. Date parsing is expensive, so reuse your formatter or consider using `DateComponents` for repeated tasks.

## See Also

- [NSDateFormatter - Apple Developer](https://developer.apple.com/documentation/foundation/nsdateformatter)
