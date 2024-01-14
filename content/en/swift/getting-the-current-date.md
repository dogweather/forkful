---
title:    "Swift recipe: Getting the current date"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why
Have you ever needed to know the current date in your Swift program? Whether you're building a scheduling app or simply displaying the current date in a user interface, getting the current date is a common task in programming.

## How To
Fortunately, Swift makes it easy to get the current date with just a few lines of code and some built-in functions. Let's take a look at how to get the current date and format it in various ways.

```Swift
// Import the Foundation framework
import Foundation

// Get the current date and assign it to a Date variable
let currentDate = Date()

// Print the current date in the format "Month Day, Year"
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "MMMM dd, yyyy"
let formattedDate = dateFormatter.string(from: currentDate)
print(formattedDate) // Output: March 23, 2021

// Get the current date and time in the format "Month Day, Year - Hour:Minutes"
dateFormatter.dateFormat = "MMMM dd, yyyy - HH:mm"
let formattedDateTime = dateFormatter.string(from: currentDate)
print(formattedDateTime) // Output: March 23, 2021 - 18:23

// Get the current day of the week
let calendar = Calendar.current
let weekDay = calendar.component(.weekday, from: currentDate)
print(weekDay) // Output: 3 (corresponding to Tuesday)

```

The first step is to import the Foundation framework, which contains the Date and DateFormatter classes. Then, we use the Date class to get the current date and assign it to a variable. Notice that Swift automatically assigns the current date and time as the default value, so we don't need to specify any parameters.

Next, we use the DateFormatter class to format the current date in the desired format. In our first example, we set the date format to "MMMM dd, yyyy", which stands for the full month name (March), the day of the month (23), and the four-digit year (2021).

In our second example, we add the time to the date format by using "HH:mm". Notice that we use two "H" characters for the hour (24-hour format) and two "m" characters for the minutes.

Finally, we can use the Calendar class to get more specific information about the current date, such as the day of the week. In our example, we use the "weekday" component to get a number corresponding to Tuesday (3).

## Deep Dive
Behind the scenes, Swift is using the Foundation framework's NSCalendar and NSDateFormatter classes to get the current date and format it. These classes are based on the Gregorian calendar, which is the most widely used calendar system in the world.

Additionally, we can manipulate the current date using the DateComponents class to get specific components (year, month, day, etc.) and modify them as needed.

## See Also
- [Apple Developer Documentation - Date](https://developer.apple.com/documentation/foundation/date)
- [Apple Developer Documentation - DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
- [Apple Developer Documentation - Calendar](https://developer.apple.com/documentation/foundation/calendar)
- [NSHipster - Dates and Times in Swift](https://nshipster.com/dateformatter/)