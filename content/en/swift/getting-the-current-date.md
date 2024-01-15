---
title:                "Getting the current date"
html_title:           "Swift recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why
Have you ever needed to display the current date in your Swift program, whether it be for a calendar app or a reminder system? Knowing how to retrieve the current date and time is an essential skill for any Swift developer.

## How To
Getting the current date in Swift is a simple process. First, we need to import the Foundation framework, which contains the Date class.

```
import Foundation
```

Then, we can create an instance of the Date class and assign it to a variable.

```
let currentDate = Date()
```

To display the date in a specific format, we can use the DateFormatter class. For example, to display the date in "MMMM dd, yyyy" format (e.g. January 1, 2022), we would use the following code:

```
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "MMMM dd, yyyy"
let formattedDate = dateFormatter.string(from: currentDate)

print(formattedDate) // Output: January 1, 2022
```

We can also get the current time by specifying the time format in the DateFormatter class.

```
let timeFormatter = DateFormatter()
timeFormatter.dateFormat = "hh:mm a"
let formattedTime = timeFormatter.string(from: currentDate)

print(formattedTime) // Output: 05:30 PM
```

## Deep Dive
Behind the scenes, the Date class actually stores the number of seconds since January 1st, 2001 at 12:00 AM UTC. So, when we create an instance of the Date class without any parameters, we are essentially getting the current date and time in seconds.

The DateFormatter class allows us to format the date and time according to our preferences. We can specify the formatting using various characters, such as "M" for the month, "d" for the day, "y" for the year, "h" for the hour, "m" for the minute, and "a" for am/pm. It's important to note that these characters are case-sensitive.

## See Also
- [Apple's Documentation on the Date Class](https://developer.apple.com/documentation/foundation/date)
- [DateFormatter Formatting Guide](https://www.nsdateformatter.com/)
- [Swift Date Formatting Tutorial](https://www.hackingwithswift.com/articles/153/how-to-use-dateformatter-to-format-dates-and-times)