---
title:                "Rounding numbers"
date:                  2024-01-25T03:00:21.613217-07:00
model:                 gpt-4-1106-preview
simple_title:         "Rounding numbers"

category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/rounding-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Rounding numbers is chopping off the digits beyond a certain point while optionally adjusting the last kept digit. Programmers round to reduce precision when exact values aren't necessary, manage floating-point errors, or prep numbers for user-friendly display.

## How to:
In C, you'd typically use `floor()`, `ceil()`, or `round()` functions. Here's the quick show:

```C
#include <stdio.h>
#include <math.h>

int main() {
    double num = 3.14159;
    double num_floor = floor(num);
    double num_ceil = ceil(num);
    double num_round = round(num);

    printf("Floor: %.2f\n", num_floor); // Floor: 3.00
    printf("Ceil: %.2f\n", num_ceil);   // Ceil: 4.00
    printf("Round: %.2f\n", num_round); // Round: 3.00
    return 0;
}
```

For more control, like rounding to a specific place, you multiply, round, and divide:

```C
double roundToPlace(double num, int place) {
    double scale = pow(10.0, place);
    return round(num * scale) / scale;
}

// ...

double num = 3.14159;
double num_rounded = roundToPlace(num, 2);
printf("Rounded to 2 decimal places: %.2f\n", num_rounded); // Rounded to 2 decimal places: 3.14
```

## Deep Dive
Back in the day, rounding often meant a manual process—a heavy lift with just pen and paper. With computing, we automated this, but floating-point arithmetic brought nuances due to its binary nature, where some numbers can't be represented exactly.

Alternatives to standard rounding include truncation (simply dropping extra digits) or bankers' rounding, which rounds to the nearest even number when exactly between two values, reducing bias in repeated calculations.

Implementation gets tricky when you need to round arbitrary precision numbers or handle special cases like infinity, signaling NaNs, or subnormal values. The C standard library functions handle the basics, but if you need to round decimals in custom ways, you'll need more than `math.h`.

## See Also
- [`<math.h>` documentation](https://en.cppreference.com/w/c/numeric/math)
- [Floating-point arithmetic](https://en.wikipedia.org/wiki/Floating-point_arithmetic)
- [The pitfalls of verifying floating-point computations](https://dl.acm.org/doi/10.1145/1186736.1186737)
