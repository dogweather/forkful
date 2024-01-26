---
title:                "Rounding numbers"
date:                  2024-01-25T02:59:55.932693-07:00
model:                 gpt-4-1106-preview
simple_title:         "Rounding numbers"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/rounding-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

Rounding numbers means approximating a numerical value to a specific precision, typically to remove unwanted decimals. Programmers round to manage memory, improve readability, and meet domain-specific requirements like currency constraints.

## How to:

Swift provides several ways to round numbers. Here's a taste:

```Swift
let original = 3.14159

// Standard rounding
let standardRounded = round(original) // 3.0

// Rounding to specific decimal place
let decimalRounded = Double(round(original * 1000) / 1000) // 3.142

// Rounding down
let roundedDown = floor(original) // 3.0

// Rounding up
let roundedUp = ceil(original) // 4.0

print("Standard: \(standardRounded), Decimal: \(decimalRounded), Down: \(roundedDown), Up: \(roundedUp)")
```

Output: `Standard: 3.0, Decimal: 3.142, Down: 3.0, Up: 4.0`

## Deep Dive

Historically, rounding is a mathematical concept predating computers, essential in commerce and science. Swift's `Foundation` framework offers comprehensive rounding functionality:

- `round(_: )` is good old half-up rounding.
- `floor(_: )` and `ceil(_: )` handle directional rounding.
- `rounded(.up/.down/.toNearestOrAwayFromZero)` gives finer control with rounding rules enum.

Be aware of the `Decimal` type for precise financial calculations, which avoids floating-point errors. Also, explore `NSDecimalNumber` for Objective-C compatibility.

## See Also

- IEEE Standard for Floating-Point Arithmetic (IEEE 754): [IEEE 754](https://ieeexplore.ieee.org/document/4610935)
