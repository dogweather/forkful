---
title:                "Rounding a number"
date:                  2024-01-24T20:57:22.595903-07:00
model:                 gpt-4-1106-preview
simple_title:         "Rounding a number"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/rounding-a-number.md"
---

{{< edit_this_page >}}

## What & Why?

Rounding a number means adjusting it to its nearest whole value or to a certain number of decimal places. Programmers round numbers to simplify calculations, improve readability, or meet certain numerical precision requirements in their applications.

## How to:

Rounding in Kotlin is straightforward and comes with built-in functions. Here's a quick look with examples:

```ProgLang.KOTLIN
fun main() {
    val number = 3.14159

    val roundedToZeroDecimal = kotlin.math.round(number)
    println(roundedToZeroDecimal) // Output: 4
    
    val roundedToTwoDecimals = String.format("%.2f", number).toDouble()
    println(roundedToTwoDecimals) // Output: 3.14
    
    val roundedToInt = number.toInt() // Note that this truncates, not rounds
    println(roundedToInt) // Output: 3
}
```

## Deep Dive

Rounding has been around as long as we've been doing math. It's not unique to Kotlin or even programming. In Kotlin, `kotlin.math.round` function rounds a number to the nearest integer value. If you need to round to specific decimal places, Kotlin doesn't provide a direct standard library function, but you can use `String.format` or `BigDecimal` for more precision control.

Alternatives for rounding in Kotlin include:

- `Math.floor` which rounds down to the nearest whole number.
- `Math.ceil` which rounds up to the nearest whole number.
- `BigDecimal` for precise calculations and control over rounding modes.

It's important to note the difference between rounding and truncating. The method `toInt()` simply truncates the decimal without rounding. So 3.9 would become 3, not 4.

## See Also

- The official Kotlin documentation on number types: https://kotlinlang.org/docs/basic-types.html#numbers
- An in-depth guide to rounding with `BigDecimal`: https://docs.oracle.com/javase/8/docs/api/java/math/BigDecimal.html
- The Kotlin API reference for `kotlin.math.round`: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.math/round.html