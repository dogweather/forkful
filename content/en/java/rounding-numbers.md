---
title:                "Rounding numbers"
date:                  2024-01-25T03:00:12.757467-07:00
model:                 gpt-4-1106-preview
simple_title:         "Rounding numbers"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/rounding-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Rounding numbers means adjusting them to a specified degree of precision. Programmers do it to simplify numbers for readability, to meet certain specifications, or to ensure calculations fit within certain bounds, like avoiding precision errors in floating-point arithmetic.

## How to:
Java offers multiple ways to round numbers. Here’s a quick demo with `Math.round()`, `BigDecimal`, and `DecimalFormat`.

```java
public class RoundingDemo {
    public static void main(String[] args) {
        double num = 123.4567;

        // Using Math.round()
        long roundedNum = Math.round(num);
        System.out.println(roundedNum); // Output: 123

        // Using BigDecimal for more control
        BigDecimal bd = new BigDecimal(num).setScale(2, RoundingMode.HALF_UP);
        double roundedBigDecimal = bd.doubleValue();
        System.out.println(roundedBigDecimal); // Output: 123.46

        // Using DecimalFormat
        DecimalFormat df = new DecimalFormat("#.##");
        String formattedNum = df.format(num);
        System.out.println(formattedNum); // Output: 123.46
    }
}
```

## Deep Dive
Historically, rounding numbers has been essential for analog calculations and has carried over to digital computing for efficiency and accuracy. Rounding errors, like those from floating-point arithmetic, demonstrate that this isn't a trivial issue — they can accumulatively mess up calculations in, say, aerospace and financial applications.

Beyond `Math.round()`, you’ve got `BigDecimal`, which gives you finer control over the scale and rounding mode, and `DecimalFormat` for when you need to round numbers as part of formatting text output. Alternatives to rounding include flooring, ceiling, and truncating, which are different ways to handle precision and typically handled by various `Math` methods.

Depending on your use case, the rounding strategy may vary. For example, `BigDecimal` is go-to for financial calculations, where precision is critical. In contrast, `Math.round()` is a quick way for general-purpose operations where you’re less picky about the rounding mode.

## See Also
- [Oracle's Java Math documentation](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/Math.html)
- [IEEE Standard for Floating-Point Arithmetic (IEEE 754)](https://ieeexplore.ieee.org/document/4610935)
- [DecimalFormat Class in Java](https://docs.oracle.com/javase/7/docs/api/java/text/DecimalFormat.html)