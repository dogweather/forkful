---
title:                "Rounding a number"
date:                  2024-01-24T20:58:13.387773-07:00
model:                 gpt-4-1106-preview
simple_title:         "Rounding a number"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/rounding-a-number.md"
---

{{< edit_this_page >}}

## What & Why?
Rounding a number is the process of adjusting its value to the nearest whole number or a specified level of precision. Programmers round numbers to simplify values, particularly when dealing with inexact measurements or when an exact value is not necessary for the given context.

## How to:
In Arduino, rounding is straightforward. You can use functions like `round()`, `ceil()`, or `floor()` depending on your needs. Here's how you'd use each in your code:

```arduino
void setup() {
  Serial.begin(9600); // Start the serial communication
  
  float myNumber = 3.14;

  Serial.print("Original number: ");
  Serial.println(myNumber);

  // Standard rounding to nearest whole number
  Serial.print("Rounded number: ");
  Serial.println(round(myNumber)); // Outputs 3

  // Rounding up (ceiling)
  Serial.print("Ceiling number: ");
  Serial.println(ceil(myNumber)); // Outputs 4

  // Rounding down (floor)
  Serial.print("Floored number: ");
  Serial.println(floor(myNumber)); // Outputs 3
}

void loop() {
  // Nothing to do here
}
```

In the above code, we see different results for each function, highlighting their varying behaviors.

## Deep Dive
Rounding a number isn't just a modern computing task—it's an age-old mathematical operation. In programming, the necessity to round numbers arises from the fact that digital computers work with a finite level of precision. This is particularly true for floating-point numbers, where certain calculations can yield results with many decimal places or results that need to fit a specific format.

The Arduino language provides several built-in functions for rounding numbers:

- `round()`: Rounds to the nearest whole number using the "half up" strategy, meaning that it rounds to the nearest integer away from zero if the fractional part is 0.5 or greater.
- `ceil()`: Stands for "ceiling," which rounds a number up to the closest integer that is greater than or equal to it, regardless of the fractional part.
- `floor()`: The opposite of `ceil()`, this function rounds a number down to the closest integer that is less than or equal to it, disregarding the fractional part.

When choosing which function to use, consider the direction of rounding you need based on the application. For financial calculations, standard rounding (using `round()`) is common. However, depending on your rounding rules and accuracy needs, you might consider writing custom rounding functions or leveraging other libraries for more control.

## See Also
For further reading and resources related to rounding numbers and precision in programming, you might want to check out the following:

- [Arduino Reference: round() function](https://www.arduino.cc/reference/en/language/functions/math/round/)
- [Arduino Reference: ceil() function](https://www.arduino.cc/reference/en/language/functions/math/ceil/)
- [Arduino Reference: floor() function](https://www.arduino.cc/reference/en/language/functions/math/floor/)
- [IEEE Standard for Floating-Point Arithmetic (IEEE 754)](https://ieeexplore.ieee.org/document/4610935)
- [A Guide to Numeric Types in Arduino](https://create.arduino.cc/projecthub/arduino_genuino/an-introduction-to-arduino-numeric-types-decimals-eea5f2)