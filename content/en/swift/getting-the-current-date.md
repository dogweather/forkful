---
title:    "Swift recipe: Getting the current date"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Why
In any programming language, being able to retrieve the current date is a fundamental function. Whether it’s for displaying the date on a UI or for data manipulation, having the current date is essential for many applications. In Swift, getting the current date is simple and can be done with just a few lines of code.

## How To
To get the current date in Swift, we will be using the `Date` class from the `Foundation` framework. Here’s a basic example of how to retrieve the current date and print it to the console:

```Swift
// Import Foundation framework
import Foundation

// Create an instance of Date using the current date and time
let currentDate = Date()

// Create an instance of DateFormatter to format the date
let dateFormatter = DateFormatter()

// Set the date format to MM/dd/yyyy
dateFormatter.dateFormat = "MM/dd/yyyy"

// Convert the date to a string
let stringDate = dateFormatter.string(from: currentDate)

// Print the date to the console
print(stringDate) // Output: "01/01/2020"
```

The `DateFormatter` class allows us to format the date in any way we want. In the example above, we used the `dateFormat` property to specify the format we want to use. You can change the format to suit your needs, such as displaying the time along with the date, or using a different date format.

## Deep Dive
Under the hood, the `Date` class represents a specific point in time that is independent of time zones or calendars. It uses the UTC (Coordinated Universal Time) time zone to ensure consistency across different devices and regions. This means that no matter where you are in the world, the same `Date` object will always represent the same point in time.

It’s important to note that getting the current date is not the same as getting the current time. The `Date` class stores the current date and time accurate to the millisecond, but it does not include information about the current time zone. If you want to know the current time in your local time zone, you will need to use the `TimeZone` class in conjunction with the `Date` class.

## See Also
- [Apple Developer Documentation - Date](https://developer.apple.com/documentation/foundation/date)
- [Apple Developer Documentation - DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
- [Apple Developer Documentation - TimeZone](https://developer.apple.com/documentation/foundation/timezone)