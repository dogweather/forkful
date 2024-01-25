---
title:                "Rounding a number"
date:                  2024-01-24T20:57:30.199927-07:00
model:                 gpt-4-1106-preview
simple_title:         "Rounding a number"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/rounding-a-number.md"
---

{{< edit_this_page >}}

## What & Why?

Rounding a number is the process of adjusting it to its nearest whole number or to a particular decimal place. Programmers round numbers to simplify calculations, present more readable data, or meet certain mathematical requirements.

## How to:

Swift provides a few straightforward approaches for rounding numbers. Below, you'll find examples of how to perform common rounding operations:

```swift
let originalNumber = 3.14159

// Rounding to the nearest whole number
let roundedNumber = round(originalNumber)
print(roundedNumber) // Output: 3

// Rounding down
let roundedDown = floor(originalNumber)
print(roundedDown) // Output: 3

// Rounding up
let roundedUp = ceil(originalNumber)
print(roundedUp) // Output: 4

// Rounding to a specified number of decimal places
let multiplier = pow(10.0, Double(2)) // 100 for 2 decimal places
let roundedToDecimal = round(originalNumber * multiplier) / multiplier
print(roundedToDecimal) // Output: 3.14
```

## Deep Dive

Rounding numbers is not unique to Swift; it's a common feature in many programming languages, reflecting its importance in a range of applications, from financial computations to graphics rendering. Historically, rounding has been handled in various ways, from truncation (simply chopping off extra digits) to more sophisticated mathematical rounding rules.

In Swift, aside from the straightforward `round`, `floor`, and `ceil` functions, there are more advanced options. For instance, you can use the `Decimal` type, which provides the `rounding(accordingToBehavior:)` method where you can specify the rounding rules in detail, giving you control over round-up or round-down behaviors, and how to handle rounding at midpoints.

An alternative type that's good to know about is `NSDecimalNumber` when working within the Cocoa framework. It also provides customizable rounding behaviors and is convenient for locale-specific currency rounding.

As for implementation, Swift's rounding functions handle both positive and negative numbers, and the choice between `floor` and `ceil` does matter based on the direction of rounding you need. Moreover, remember when rounding floating-point numbers, due to their inherent inaccuracy, the result might not always be mathematically exact.

## See Also

- For more on formatting numbers, Apple's String Formatting Guide can be helpful, found at https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/Strings/Articles/formatSpecifiers.html.
- The official Swift documentation on numbers and their manipulation is a good resource: https://docs.swift.org/swift-book/LanguageGuide/Numbers.html.
- To explore the Decimal type and its precision rounding methods in detail, check out the Swift Standard Library documentation at: https://developer.apple.com/documentation/foundation/decimal.