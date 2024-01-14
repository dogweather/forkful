---
title:                "Swift recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why
Have you ever needed to know the current date in your Swift programming? Whether it's for displaying the date in a user interface or for calculating time intervals, getting the current date is a valuable skill to have as a Swift programmer.

## How To
To get the current date in Swift, we can use the `Date()` function. This returns the current date and time in our code. Let's take a look at a simple example:
```
let currentDate = Date()
print(currentDate)
```
The output of this code would be the current date and time in your console. But what if you want to format the date in a specific way? We can use `DateFormatter` to do this. Let's say we want to display the date in the format: "Monday, September 13, 2021". Here's how we can do that:
```
let formatter = DateFormatter()
formatter.dateFormat = "EEEE, MMMM d, yyyy"
let formattedDate = formatter.string(from: Date())
print(formattedDate)
```
The output of this code would be the current date in the format we specified. You can also change the format to suit your needs, such as displaying just the day or just the month. Play around with the `dateFormat` property to see what works best for your project.

## Deep Dive
`Date()` may seem simple on the surface, but there's actually a lot more going on under the hood. Swift uses a data type called `TimeInterval` to represent dates and times, which is the number of seconds since January 1, 2001 at 00:00:00 UTC. This is commonly referred to as the Unix timestamp. The `Date()` function converts this timestamp into a human-readable date and time.

Another important thing to note is that the date and time returned by `Date()` is based on the current timezone of the device running the code. This means that the date and time could be different for someone in a different timezone.

## See Also
Here are some links for more information about working with dates in Swift:

- [Apple's documentation on `Date`](https://developer.apple.com/documentation/foundation/date)
- [Working with Dates and Times in Swift - NSHipster](https://nshipster.com/date/)
- [Formatting Dates in Swift using DateFormatter - Hacking with Swift](https://www.hackingwithswift.com/example-code/system/how-to-format-dates-inside-text-views)