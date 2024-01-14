---
title:    "Swift recipe: Converting a date into a string"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Why
Converting a date into a string may seem like a simple task, but it is an essential aspect of programming that can have a significant impact on user experience. By converting a date into a string, developers can display dates in a more user-friendly and readable format, making their programs more accessible and intuitive.

## How To
To convert a date into a string, we can use the `DateFormatter` class in Swift. This class allows us to specify the format in which we want to display the date. Let's take a look at an example:

```Swift
let currentDate = Date() //get current date
let dateFormatter = DateFormatter() //initialize object of DateFormatter class
dateFormatter.locale = Locale(identifier: "en_US_POSIX") //set desired locale
dateFormatter.dateFormat = "MMM d, yyyy" //set desired date format
let dateString = dateFormatter.string(from: currentDate) //convert date to string
print(dateString) //output: Aug 12, 2021
```

In the above code, we first get the current date using the `Date()` function. Then, we initialize an object of the `DateFormatter` class and specify the language locale and date format we want to use. Finally, we use the `string(from:)` method to convert the date object into a string. The output will be in the specified format.

## Deep Dive
The `dateFormat` property of the `DateFormatter` class allows us to set a custom date format according to our needs. This format is made up of a combination of different symbols that represent different components of the date, such as year, month, day, hour, minute, etc. Some commonly used symbols are `yyyy` for 4-digit year, `MM` for 2-digit month, `dd` for 2-digit day, `hh` for 2-digit hour in 12-hour format, and `HH` for 2-digit hour in 24-hour format. A full list of symbols and their meanings can be found in the Apple Developer Documentation.

## See Also
- [DateFormatter Class - Apple Developer Documentation](https://developer.apple.com/documentation/foundation/dateformatter)
- [Date and Time Programming Guide - Apple Developer Documentation](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/DatesAndTimes/DatesAndTimes.html)
- [Unicode Technical Standard #35: Date Format Patterns - Unicode Consortium](https://unicode.org/reports/tr35/tr35-dates.html#Date_Field_Symbol_Table)