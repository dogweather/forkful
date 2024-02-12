---
title:                "Rounding numbers"
aliases:
- /en/javascript/rounding-numbers/
date:                  2024-01-25T03:00:07.457967-07:00
model:                 gpt-4-1106-preview
simple_title:         "Rounding numbers"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/rounding-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Rounding is chopping off the noise after a certain point in a number. Programmers round to control precision, manage memory, or make output user-friendly—like turning 2.998 into a clean 3.

## How to:
Here's how you round numbers in JavaScript using `Math.round()`, `Math.ceil()`, and `Math.floor()`: 

```javascript
let originalNumber = 2.567;

let roundedDown = Math.floor(originalNumber); // 2
let roundedUp = Math.ceil(originalNumber);    // 3
let rounded = Math.round(originalNumber);     // 3 (since .567 is more than .5)

console.log(roundedDown); // Prints: 2
console.log(roundedUp);   // Prints: 3
console.log(rounded);     // Prints: 3
```

To fix to a certain number of decimal places, use `toFixed()`:

```javascript
let twoDecimals = originalNumber.toFixed(2); // "2.57" (returns a string)

console.log(twoDecimals); // Prints: "2.57"
```

Convert the string back to a number with a unary plus or `Number()`:

```javascript
let numberAgain = +twoDecimals; // 2.57

console.log(numberAgain); // Prints: 2.57
```

## Deep Dive
Rounding numbers isn't new; it's as old as numbers. In JavaScript, `Math.round()` uses "round half up" tie-breaking: if the fractional part is 0.5, it rounds to the nearest even number.

For more control, `toFixed()` might be your go-to, but remember, it returns a string. Converting back to a number might be an extra step but ensures you keep working with numeric types. 

Alternatives? Libraries like `lodash` offer `_.round(number, [precision=0])` for more nuanced control. Or, the newer `Intl.NumberFormat` gives you high-precision formatting beyond just rounding.

Speaking of precision, beware of floating-point quirks in JavaScript. `0.1 + 0.2` doesn't exactly equal `0.3` due to how numbers are stored. Sometimes, rounding becomes necessary to correct such floating-point errors.

## See Also
- Mozilla's Math documentation: [MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math)
- Financial rounding with `Intl.NumberFormat`: [ECMAScript Internationalization API](https://tc39.es/ecma402/#numberformat-objects)
- `lodash` rounding: [Lodash Docs](https://lodash.com/docs/4.17.15#round)
