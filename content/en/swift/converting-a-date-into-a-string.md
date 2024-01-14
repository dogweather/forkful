---
title:                "Swift recipe: Converting a date into a string"
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why
Converting a date into a string is a common task in Swift programming that allows you to display dates in a specific format or use them in API calls. It is important to understand how to properly convert a date into a string in order to effectively work with dates in your code.

## How To
Converting a date into a string in Swift is relatively straightforward. Here are two ways to do it:

### Using DateFormatter
``` Swift
let date = Date()
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "E, d MMM yyyy"
let dateString = dateFormatter.string(from: date)

```
Output: "Wed, 18 Aug 2021"

In this example, we first create a Date object which represents the current date and time. We then create a DateFormatter object and set the dateFormat property to the desired format of the date string. Finally, we use the string(from:) method to convert the date into a string according to the specified format.

### Using DateComponents and Calendar
```Swift
let date = Date()
let calendar = Calendar.current
let components = calendar.dateComponents([.day, .month, .year], from: date)
let day = components.day
let month = components.month
let year = components.year
let dateString = "\(day ?? 0) \(month ?? 0) \(year ?? 0)"

```
Output: "18 8 2021"

In this example, we first create a Date object and then use the current calendar to extract the day, month, and year components from the date. We then use string interpolation to construct a date string in the desired format.

## Deep Dive
When converting a date into a string, it is important to consider localization and timezones. DateFormatter has methods to handle localization, such as setting the locale to the user's preferred locale. It also has options to display the time along with the date or to convert the time to a specific timezone.

When using DateComponents and Calendar, it is important to understand the differences between .day, .month, and .year components based on the calendar being used. Some calendars, like the Islamic or Hebrew calendars, have different definitions for these components and may result in unexpected conversions.

Overall, understanding the intricacies of converting a date into a string and utilizing the appropriate methods and options can greatly improve the functionality and accuracy of your code.

## See Also
- [DateFormatter Class Reference](https://developer.apple.com/documentation/foundation/dateformatter)
- [Calendar Class Reference](https://developer.apple.com/documentation/foundation/calendar)
- [DateComponents Class Reference](https://developer.apple.com/documentation/foundation/datecomponents)