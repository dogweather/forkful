---
title:    "Swift recipe: Converting a date into a string"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why
In Swift, dates and strings are commonly used data types and often need to be converted into each other for various purposes. In this blog post, we will explore how to convert a date into a string, which can be useful for tasks such as displaying the date in a user-friendly format or saving it in a database.

## How To

To convert a date into a string, we will use a method called `dateFormatter()` from the `Foundation` framework. This method allows us to specify the format in which we want the date to be displayed.

Let's take a look at an example:

```Swift
import Foundation

let date = Date() // current date
let dateFormatter = DateFormatter()

dateFormatter.dateFormat = "dd/MM/yyyy" // specifying date format
let dateString = dateFormatter.string(from: date) // converting date into a string
print(dateString) // output: 21/03/2021
```

In the code above, we first import the `Foundation` framework to access the `Date` and `DateFormatter` data types. Then, we initialize a `Date` variable to hold the current date. Next, we create an instance of `DateFormatter` and specify the desired date format using `dateFormat`. Finally, we convert the date into a string using the `string(from:)` method and print the result.

We can also customize the date format even further by adding other elements such as time or time zone. For example, if we want our string to include the time as well, we can use the format `dd/MM/yyyy HH:mm:ss`.

## Deep Dive

Behind the scenes, the `dateFormat` property of `DateFormatter` uses a special syntax to represent the elements of a date. For example, `dd` represents the day, `MM` represents the month, and `yyyy` represents the year. We can also use other symbols such as slashes or hyphens to separate the date elements.

It is important to use the correct syntax when specifying the date format, otherwise, the conversion may not work as expected. For a complete list of available symbols, you can refer to the [Unicode Technical Standard #35](https://www.unicode.org/reports/tr35/tr35-31/tr35-dates.html#Date_Field_Symbol_Table).

Additionally, we can also use the `Locale` property of `DateFormatter` to specify the language or region for the date format. This allows for a more customized and localized output.

## See Also
- [Apple's Date Formatting Guide](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/DataFormatting/Articles/dfDateFormatting10_4.html)
- [Swift Date and DateFormatter Tutorial](https://medium.com/@luisferes/amazing-dates-in-swift-237a5254209c)
- [Unicode Technical Standard #35](https://www.unicode.org/reports/tr35/tr35-31/tr35-dates.html#Date_Field_Symbol_Table)