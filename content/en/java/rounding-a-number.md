---
title:                "Rounding a number"
date:                  2024-01-24T20:57:36.635912-07:00
model:                 gpt-4-1106-preview
simple_title:         "Rounding a number"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/rounding-a-number.md"
---

{{< edit_this_page >}}

## What & Why?
Rounding a number often involves reducing the amount of decimal places to a more manageable figure, often for readability, adhering to certain mathematical rules, or for practical application like currency conversion. Programmers round numbers to avoid unwieldy precision and improve user-friendliness of numerical data.

## How to:
Java provides multiple ways to round a number. You can use `Math.round()`, `BigDecimal`, or even format strings depending on the situation. Here are some examples:

```java
public class RoundNumberExample {
    public static void main(String[] args) {
        
        double original = 123.4567;

        // Using Math.round to round to the nearest whole number
        long roundOff1 = Math.round(original);
        System.out.println(roundOff1); // 123

        // Using Math.round to round to a specific number of decimal places
        double roundOff2 = Math.round(original * 100.0) / 100.0;
        System.out.println(roundOff2); // 123.46
        
        // Using BigDecimal for precise rounding modes
        BigDecimal bd = new BigDecimal(original).setScale(2, RoundingMode.HALF_UP);
        double roundOff3 = bd.doubleValue();
        System.out.println(roundOff3); // 123.46
        
        // Using DecimalFormat for formatting output
        DecimalFormat df = new DecimalFormat("#.00");
        String roundOff4 = df.format(original);
        System.out.println(roundOff4); // 123.46
    }
}
```

Sample output:
```
123
123.46
123.46
123.46
```

## Deep Dive
Rounding numbers is as old as mathematics. Throughout history, various rounding systems have developed depending on cultural and practical demands. In computer programming, rounding is a common task as digital calculations can produce very long decimal places that are often unnecessary and impractical to display or utilize in further computations.

Java, as a strongly typed programming language, provides multiple utilities for rounding numbers. `Math.round()` is a quick method that returns the nearest whole number, but it doesn't allow you to specify the number of decimal places. When you need precision, `BigDecimal` is the preferred class because it's designed for accuracy, providing various rounding modes defined in `RoundingMode` enum – such as `HALF_UP`, `HALF_DOWN`, and `CEILING`. Although `BigDecimal` gives a higher level of control, it comes at a cost of less straightforward syntax and performance overhead due to its comprehensive representation of numbers.

Lastly, `DecimalFormat` is more about representing numbers with rounded figures rather than changing their values. This class is useful for presenting numbers in a human-readable format especially when you don't need to change the original value itself but just its representation.

## See Also
- Oracle JavaDocs on BigDecimal: https://docs.oracle.com/javase/8/docs/api/java/math/BigDecimal.html
- Oracle JavaDocs on DecimalFormat: https://docs.oracle.com/javase/8/docs/api/java/text/DecimalFormat.html
- Oracle JavaDocs on Math: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html