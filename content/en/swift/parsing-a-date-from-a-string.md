---
title:                "Parsing a date from a string"
html_title:           "Swift recipe: Parsing a date from a string"
simple_title:         "Parsing a date from a string"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing a date from a string in Swift is the process of converting a date represented as a string into a date object that can be used in the code. Programmers use this technique to handle date and time data coming from various sources, such as user input or external APIs.

## How to:

```Swift
// Example 1: Parsing a date from a string using DateFormatter

let dateString = "2021-04-20" // string representation of a date

let formatter = DateFormatter()
formatter.dateFormat = "yyyy-MM-dd" // specify the format of the input string

if let date = formatter.date(from: dateString) { // try to convert the string to a date object
    print(date) // output: 2021-04-20 00:00:00 +0000
} else {
    print("Unable to parse date") // handle error if conversion fails
}

// Example 2: Parsing a date with time using ISO8601DateFormatter

let dateTimeString = "2021-04-20T10:30:00+0000" // string representation of a date with time

let isoFormatter = ISO8601DateFormatter()
isoFormatter.formatOptions = [.withInternetDateTime] // specify the format of the input string
isoFormatter.timeZone = TimeZone(abbreviation: "UTC") // set the time zone of the input string

if let dateTime = isoFormatter.date(from: dateTimeString) { // try to convert the string to a date object
    print(dateTime) // output: 2021-04-20 10:30:00 +0000
} else {
    print("Unable to parse date and time") // handle error if conversion fails
}
```

## Deep Dive:

Parsing dates from strings has a long history in programming, with various techniques and formats used throughout the years. Before Swift, NSDateFormatter was used to parse dates in Objective-C. In Swift, DateFormatter and ISO8601DateFormatter are commonly used, providing powerful options for handling various date formats. Alternatives to parsing dates include using libraries that provide built-in date handling capabilities, such as Foundation's Calendar and DateComponents classes. Implementing a custom date parser is another option, but it should only be considered for specialized cases as it can be error-prone.

## See Also:

- [NSDateFormatter Documentation](https://developer.apple.com/documentation/foundation/nsdateformatter)
- [ISO8601DateFormatter Documentation](https://developer.apple.com/documentation/foundation/iso8601dateformatter)
- [Foundation Calendar Documentation](https://developer.apple.com/documentation/foundation/calendar)
- [Foundation DateComponents Documentation](https://developer.apple.com/documentation/foundation/datecomponents)