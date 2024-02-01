---
title:                "Rounding numbers"
date:                  2024-02-01T13:47:36.877171-07:00
model:                 gpt-4-0125-preview
simple_title:         "Rounding numbers"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/google-apps-script/rounding-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

Rounding numbers is all about changing a number with a long decimal tail to a simpler, shorter form. Programmers do this to make numbers easier to read, manage, and sometimes to comply with mathematical rules or constraints within their applications.

## How to:

Google Apps Script, being based on JavaScript, offers a handful of ways to round numbers. Let's dive into some common methods: `Math.round()`, `Math.floor()`, and `Math.ceil()`, along with a trick for rounding to a specific number of decimal places.

```Google Apps Script
// Rounding to the nearest whole number
var number = 2.56;
var rounded = Math.round(number); // Outputs: 3

// Rounding down
var numberDown = 2.99;
var floored = Math.floor(numberDown); // Outputs: 2

// Rounding up
var numberUp = 2.01;
var ceiled = Math.ceil(numberUp); // Outputs: 3

// Rounding to a specific number of decimal places
// Let's say we want to round 2.34567 to 2 decimal places
var specificRounded = Math.round(2.34567 * 100) / 100; // Outputs: 2.35

// Logging out the results
console.log("Rounded:", rounded);
console.log("Floored:", floored);
console.log("Ceiled:", ceiled);
console.log("Specific Rounded:", specificRounded);
```

Note that the trick for rounding to a specific number of decimal places involves multiplying the number by 10 raised to the power of desired decimal places (`Math.round(number * 10**digits) / 10**digits`), rounding it, then dividing it back. This technique can be a bit clunky, but it's straightforward and works.

## Deep Dive

Rounding numbers is a basic but crucial operation in programming, enabling data to fit more neatly in UIs, reports, and calculations. The techniques provided by JavaScript, which Google Apps Script inherits, are broadly sufficient for most purposes.

Historically, developers coming from other languages might miss more direct methods to round to a specific number of decimal places, like a built-in `round()` function that takes the number of decimals as an argument. While JavaScript's approach might seem indirect, it's versatile and fits within the larger ethos of the language—favoring a smaller set of flexible functions over a larger number of highly specific ones.

For high-precision arithmetic operations, including rounding, some developers turn to libraries like BigNumber.js or decimal.js. These libraries offer more control and precision, especially necessary in financial applications or where JavaScript's floating-point arithmetic falls short. However, for most day-to-day rounding needs within Google Apps Script, the native Math methods are more than adequate.
