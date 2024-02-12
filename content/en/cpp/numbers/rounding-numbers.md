---
title:                "Rounding numbers"
aliases: - /en/cpp/rounding-numbers.md
date:                  2024-01-25T03:00:00.122618-07:00
model:                 gpt-4-1106-preview
simple_title:         "Rounding numbers"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/rounding-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Rounding numbers means tweaking a value to its nearest integer or specified precision. Devs do it to simplify, conform to real-world constraints, or improve performance by ditching excess precision.

## How to:
C++ offers several ways to round numbers, like `floor()`, `ceil()`, and `round()`:

```C++
#include <iostream>
#include <cmath> // for rounding functions

int main() {
    double num = 3.14;

    std::cout << "floor: " << std::floor(num) << "\n"; // Output: floor: 3
    std::cout << "ceil: " << std::ceil(num) << "\n";   // Output: ceil: 4
    std::cout << "round: " << std::round(num) << "\n"; // Output: round: 3

    // For fixed precision, such as rounding to two decimals:
    double precise_num = 3.146;
    double multiplier = 100.0;
    double rounded = std::round(precise_num * multiplier) / multiplier;

    std::cout << "rounded to two decimals: " << rounded << "\n"; // Output: rounded to two decimals: 3.15

    return 0;
}
```

## Deep Dive
Before C++11, rounding relied on manual techniques or non-standard libraries. Today, `<cmath>` provides robust methods. `floor()` rounds down, `ceil()` rounds up, whereas `round()` goes to the nearest integer, even handling tie-breaking (0.5 cases) by rounding to the even number.

Understanding the behavior of these functions is crucial; for instance, negative numbers could trip you up (`std::round(-2.5)` yields `-2.0`).

Alternatives? Casting to an int after adding 0.5 for positive numbers was a classic hack but errs with negatives and isn't type-agnostic. Libraries like Boost can offer more nuanced approaches, while language extensions or compiler intrinsics can optimize for specific hardware.

## See Also
- C++ Reference for `<cmath>`: https://en.cppreference.com/w/cpp/header/cmath
- IEEE Standard for Floating-Point Arithmetic (IEEE 754): https://ieeexplore.ieee.org/document/4610935
- Boost Numeric Conversion Library: https://www.boost.org/doc/libs/release/libs/numeric/conversion/
