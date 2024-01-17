---
title:                "Converting a date into a string"
html_title:           "Swift recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why? 
Converting a date into a string means changing a date or time value into a text format. This is useful for displaying dates in a human-readable format or for storing them in a database. Programmers do this to make their apps more user-friendly and to ensure accurate date handling in their code.

## How to: 
Converting a date into a string can be done in a few simple steps in Swift. First, create a Date object using the desired date and time components. Then, use the DateFormatter class to specify the desired format for the string. Finally, use the string(from:) method to convert the Date object into a string.

Here's an example of converting today's date into a string in the format of "MM/dd/yyyy":
```Swift
let date = Date()
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "MM/dd/yyyy"
let dateString = dateFormatter.string(from: date)
print(dateString) // Output: 12/02/2021
```

## Deep Dive: 
The ability to convert dates into strings has been around since the early days of programming, as it is a fundamental requirement for date handling in applications. In addition to the DateFormatter class, there are other methods of converting dates into strings, such as using the DateComponentsFormatter class or using specific formatting styles like ISO 8601.

In some cases, developers may need to handle dates in different time zones or consider different calendar systems. Swift's Foundation framework offers robust solutions for these scenarios, such as the TimeZone and Calendar classes.

## See Also: 
To learn more about date handling in Swift, check out the official Apple documentation on Dates and Times. You can also explore more advanced features for converting dates into strings by reading about Internationalization and Localization.