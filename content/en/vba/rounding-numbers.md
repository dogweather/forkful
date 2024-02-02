---
title:                "Rounding numbers"
date:                  2024-02-01T13:31:42.556609-07:00
model:                 gpt-4-0125-preview
simple_title:         "Rounding numbers"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/vba/rounding-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Rounding numbers in VBA is about chopping off the digits you don't need from a number, making it simpler and more manageable. Programmers do this to simplify calculations, improve readability, and ensure values meet expected formats or limits.

## How to:
When it comes to rounding numbers in VBA, there are a few key functions you'll be using: `Round`, `Int`, and `Fix`. Let's see how each of these works with some quick examples.

```basic
' Using Round function
Dim roundedNumber As Double
roundedNumber = Round(3.14159, 2) ' Rounds to 2 decimal places
Debug.Print roundedNumber ' Output: 3.14

' Using Int function - Rounds down
Dim intNumber As Double
intNumber = Int(3.99)
Debug.Print intNumber ' Output: 3

' Using Fix function - Similar to Int but behaves differently with negative numbers
Dim fixedNumber As Double
fixedNumber = Fix(-3.99)
Debug.Print fixedNumber ' Output: -3
```

The `Round` function is the go-to for general rounding, `Int` always rounds down towards the nearest integer, and `Fix` is a tad more nuanced, especially with negative numbers. 

## Deep Dive
Rounding numbers seems straightforward, but it's a surprisingly complex process under the hood. The `Round` function in VBA uses bankers' rounding (round to even) as its default method. This means if the number is exactly halfway between two numbers, it rounds to the nearest even number. For example, `Round(2.5,0)` gives `2`, not `3` as you might expect if you're used to conventional rounding rules. This method reduces bias and is preferred in financial calculations.

Interestingly, while the `Round` function is versatile, you may sometimes need more control over the rounding process, especially in niche scientific or financial applications. In such cases, programmers might implement custom rounding functions using mathematical operations or explore add-ins or libraries that offer more granularity and options for roundings, such as ceiling or floor rounding methods.

While VBA's native functions cover most rounding needs, it's always good to be aware there might be cases where you'll need to venture beyond them, either through custom code or external resources.
