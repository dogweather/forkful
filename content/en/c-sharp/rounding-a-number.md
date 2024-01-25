---
title:                "Rounding a number"
date:                  2024-01-24T20:57:29.489945-07:00
model:                 gpt-4-1106-preview
simple_title:         "Rounding a number"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/rounding-a-number.md"
---

{{< edit_this_page >}}

## What & Why?

Rounding numbers means adjusting their value to the nearest whole number or a specific decimal place. Programmers do it to simplify numbers for easier readability, to make them comply with business rules, or to prepare for calculations that require integer values.

## How to:

In C#, rounding numbers is straightforward and can be accomplished with built-in methods like `Math.Round`, `Math.Floor`, and `Math.Ceiling`. Here's a quick look at how they work:

```C#
double originalNumber = 12.3456;

// Rounding to the nearest whole number
double rounded = Math.Round(originalNumber);
Console.WriteLine(rounded);  // Output: 12

// Rounding to 2 decimal places
double roundedTwoPlaces = Math.Round(originalNumber, 2);
Console.WriteLine(roundedTwoPlaces);  // Output: 12.35

// Always rounds down
double floored = Math.Floor(originalNumber);
Console.WriteLine(floored);  // Output: 12

// Always rounds up
double ceiled = Math.Ceiling(originalNumber);
Console.WriteLine(ceiled);  // Output: 13
```

## Deep Dive

Rounding numbers isn't a new need; it predates computers. Yet, in programming, how numbers are rounded can affect calculations significantly. The `Math.Round` method in C# allows for different kinds of rounding by specifying a `MidpointRounding` argument. The default behavior is to round to the nearest even number when exactly between two numbers (also known as Banker's rounding).

An alternative method for rounding specific to decimal types is `Decimal.Round`, which works much like `Math.Round`. Additionally, C# has `Math.Truncate`, which chops off the decimal portion without rounding.

The actual implementation details for how rounding works relate to the IEEE floating-point standard, which is a whole topic in itself. This standard defines how fractional numbers are represented and how operations like rounding should proceed.

## See Also

- Microsoft's documentation on `Math.Round`: [MSDN Math.Round](https://docs.microsoft.com/en-us/dotnet/api/system.math.round)
- IEEE Standard for Floating-Point Arithmetic (IEEE 754): [IEEE 754-2019](https://ieeexplore.ieee.org/document/8766229)
- More about number representation and arithmetic in C#: [Understanding Floating Point Precision, aka ‘Why does Math.Round(2.5) return 2 instead of 3?’](https://devblogs.microsoft.com/premier-developer/understanding-floating-point-precision-aka-why-does-math-round2-5-return-2-instead-of-3/)