---
title:                "Swift recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why
Converting dates into strings is a critical part of any programming task involving time. Whether you're displaying dates on a user interface or manipulating date values in your code, knowing how to convert them into strings is essential.

## How To
Converting a date into a string in Swift is a straightforward process. It involves using the `DateFormatter` class and its `string(from:)` method. Let's take a look at a simple example:

```Swift
let currentDate = Date()

let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "MMMM d, yyyy"

let stringDate = dateFormatter.string(from: currentDate)

print(stringDate) // Output: September 10, 2021
```
In the example above, we first create an instance of the `Date` class, which represents the current date and time. Next, we create a `DateFormatter` object and set its `dateFormat` property to the specified format, "MMMM d, yyyy". Finally, we use the `string(from:)` method to convert the `currentDate` into a string based on the specified format.

You can also customize the date format according to your preference. For example, if you want the day of the week to be included in the string, you can use the "EEEE" format instead. Experiment with different date formats to find one that suits your needs best.

## Deep Dive
Behind the scenes, the `DateFormatter` class uses a set of predefined Unicode patterns to format the dates. These patterns follow the syntax of the Unicode Technical Standard #35, which defines different date and time formats.

Additionally, the `DateFormatter` class provides various other options and properties that allow you to customize the date format further. For example, you can set the locale, time zone, and calendar for which the date should be formatted. These options come in handy when dealing with dates in different regions and time zones.

## See Also
- [Apple Developer Documentation - DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
- [Unicode Technical Standard #35](https://unicode.org/reports/tr35/tr35-31/tr35-dates.html)