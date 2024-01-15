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

## Why ##

Converting a date into a string is a common task in programming, especially when dealing with user input or database entries. It allows for easy manipulation and formatting of dates, making them more user-friendly and readable.

## How To ##

To convert a date into a string in Swift, we can use the `DateFormatter` class. First, we create an instance of the `DateFormatter` and set its `dateFormat` property to the desired format. Next, we use the `string(from:)` method to pass in our date object and get a string representation in return.

```Swift
// Create date formatter
let dateFormatter = DateFormatter()

// Set date format
dateFormatter.dateFormat = "MM/dd/yyyy"

// Convert date to string
let dateString = dateFormatter.string(from: Date())

print(dateString) // Outputs current date in "MM/dd/yyyy" format
```

We can customize the format by using various symbols such as "MM" for numerical month, "dd" for day, "yyyy" for year, and so on. The full list of symbols can be found in Apple's [documentation](https://developer.apple.com/documentation/foundation/dateformatter#symbols).

Additionally, we can also specify the date and time style depending on our desired output. For example, we can use `dateStyle` to get only the date, `timeStyle` to get only the time, or use both to get a combination of both.

```Swift
// Create date formatter
let dateFormatter = DateFormatter()

// Set date and time style
dateFormatter.dateStyle = .short
dateFormatter.timeStyle = .long

// Convert date to string
let dateString = dateFormatter.string(from: Date())

print(dateString) // Outputs current date and time in short and long format respectively
```

## Deep Dive ##

The `DateFormatter` class is based on the Unicode Technical Standard #35 (UTS #35), which specifies how to format date and time values to be represented as strings. The `dateFormat` property uses a customizable template string that can be tailored to fit different locales and preferences.

In addition to formatting, `DateFormatter` also handles localization, converting date and time values into different languages and regions. This is done by setting the `locale` property to the desired locale identifier. For example, if we want to get the date in Japanese, we can use the `ja_JP` locale identifier.

```Swift
// Create date formatter
let dateFormatter = DateFormatter()

// Set locale to Japanese
dateFormatter.locale = Locale(identifier: "ja_JP")

// Convert date to string
let dateString = dateFormatter.string(from: Date())

print(dateString) // Outputs current date in Japanese format
```

Another important aspect to note is that `DateFormatter` is not thread-safe. This means that it should not be used across different threads, as it could cause unexpected behavior or crashes. It is recommended to create a new instance of `DateFormatter` whenever it is needed.

## See Also ##

- Apple's [documentation](https://developer.apple.com/documentation/foundation/dateformatter)
- Tutorial on [date and time formatting](https://medium.com/@abhimuralidharan/date-and-time-formatting-in-ios-swift-4-44181332b3d4) in Swift