---
title:                "Getting the current date"
html_title:           "Elm recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?

Getting the current date in Swift is a common programming task. This is useful in applications where you need time-stamping, or to program processes based on a time schedule, for example, an alarm or a reminder system.

## How to:

Below is a simple piece of code to get the current date:

```Swift
import Foundation

let currentDate = Date()
print("Current date and time: \(currentDate)")
```

This may output something like:

``` 
Current date and time: 2023-04-17 12:06:45 +0000
```

If you just want the date, you can use:

```Swift
import Foundation

let formatter = DateFormatter()
formatter.dateStyle = .long
let dateString = formatter.string(from: Date())
print("Current date: \(dateString)")
```

This will output the date in a long format such as:

``` 
Current date: April 17, 2023
```

## Deep Dive

Swift uses the `Date` class, a part of Foundation, to manage date and time data. It's been in Swift since its birth in 2014. Before Swift, Objective-C developers relied on `NSDate`.

Getting the current date is simple, but date-time manipulation can get more complex. Be cautious with time zones and daylight-saving times. If you run services in different countries, it's essential to consider these.

There are alternatives like third-party libraries (e.g., SwiftDate), but sticking to Foundation tools is sometimes best to keep your project lightweight.

## See Also

1. [Apple Documentation – Date](https://developer.apple.com/documentation/foundation/date)

2. [Apple Documentation – DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)

3. [SwiftDate Github](https://github.com/malcommac/SwiftDate)