---
title:                "Converting a date into a string"
html_title:           "Arduino recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Converting a date into a string in Swift is transforming the date's binary form into human-readable text. Programmers do this to display dates in a user-friendly format or to smoothly transfer data between systems that use different date formats.

## How to:

Let's get straight into it. Swift offers a handy `DateFormatter` class. 

Here's a quick example:

```Swift
import Foundation

let currentDate = Date()
let formatter = DateFormatter()

formatter.dateStyle = .full

let dateString = formatter.string(from: currentDate)

print(dateString)
```

Running this will give you something like:

`"Tuesday, March 15, 2022"`

This example turns the current date into a full-format string. You can change the style (`.full` part) to `.short`, `.medium`, or `.long` to customise your output.

## Deep Dive

Date to string conversion isn't unique to Swift; it's a common task in many programming languages. Originally, computers stored dates as binary numbers. But to humans, binary numbers are undecipherable gobbledygook—problematic when we needed to view or store dates in text files. Converting dates to strings solved this issue.

Thankfully, Swift's `DateFormatter` offers various formatting styles (.short, .medium, .long, .full) as we saw before. You can even use custom formats with the help of `dateFormat` property:

```Swift
formatter.dateFormat = "E, d MMM yyyy HH:mm:ss Z"
```

This gives us more freedom to design how we want our date represented (like `"Tue, 15 Mar 2022 11:41:50 +0000"`).

A word of caution though: Converting dates to strings will cost you some performance. Swift has to process your date and output a string, which is slower than spitting out a raw binary number. So if you're dealing with thousands or millions of dates and performance is critical, you might need to rethink your strategy.

## See Also:

- [Apple Developer Documentation for DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
 
The more you know about your tools, the more elegantly you can code. So, check out these amazing resources to deep dive into date handling in Swift. Remember, great apps are powered by great code. Happy coding!