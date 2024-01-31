---
title:                "Rounding numbers"
date:                  2024-01-25T02:59:42.587583-07:00
model:                 gpt-4-1106-preview
simple_title:         "Rounding numbers"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/rounding-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Rounding numbers is trimming a number to a specific precision. Programmers do it to control numerical output for readability, display purposes, or when specific precision is required after operations that yield floating-point results.

## How to:
Rounding in TypeScript can be done using several methods. Here's a quick run-through:

```typescript
// Math.round rounds to the nearest integer
console.log(Math.round(1.5)); // Output: 2

// Math.ceil rounds up to the nearest integer
console.log(Math.ceil(1.1)); // Output: 2

// Math.floor rounds down to the nearest integer
console.log(Math.floor(1.8)); // Output: 1

// toFixed rounds to a fixed number of decimal places
let num = 1.23456;
console.log(num.toFixed(2)); // Output: "1.23"
// Note: toFixed returns a string! Use parseFloat to convert back if needed.
console.log(parseFloat(num.toFixed(2))); // Output: 1.23
```

## Deep Dive
Back in the day, rounding was a must because of limited space and precision issues in early computers. Today, floating-point arithmetic can lead to quirky results due to how numbers are stored in binary. Alternatives to rounding include floor, ceil, and trunc (for chopping off decimals without rounding).

Internals are worth noting: `Math.round` follows "round half up" (aka "commercial rounding"), while `Math.floor` and `Math.ceil` are straightforward. `toFixed` may cause unexpected results because it returns a string, and it rounds using "round half to even" (aka "bankers rounding"), especially useful to reduce bias in rounding same numbers multiple times.

## See Also
- [MDN - Math.round()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/round)
- [MDN - Math.ceil()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/ceil)
- [MDN - Math.floor()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/floor)
- [MDN - toFixed()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toFixed)
- [IEEE Standard for Floating-Point Arithmetic (IEEE 754)](https://ieeexplore.ieee.org/document/4610935)
