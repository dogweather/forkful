---
date: 2024-01-21 21:19:04.417596-07:00
description: "Error handling in your programs catches the things unforeseen that'll\
  \ try to trip you up. You do it to keep your Arduino from having a meltdown when\
  \ the\u2026"
lastmod: '2024-03-13T22:45:00.327854-06:00'
model: gpt-4-1106-preview
summary: "Error handling in your programs catches the things unforeseen that'll try\
  \ to trip you up. You do it to keep your Arduino from having a meltdown when the\u2026"
title: Handling errors
---

{{< edit_this_page >}}

## What & Why?

Error handling in your programs catches the things unforeseen that'll try to trip you up. You do it to keep your Arduino from having a meltdown when the unexpected occurs.

## How to:

Let's say your Arduino's reading a sensor that may occasionally produce out-of-range values. Here's how you might handle that:

```Arduino
int sensorValue = analogRead(A0);

if (sensorValue >= 0 && sensorValue <= 1023) {
  // Value is within range, proceed with processing
  Serial.println(sensorValue);
} else {
  // Value is out of range, handle the error
  Serial.println("Error: Sensor value out of range.");
}
```
Sample Output:
```
523
Error: Sensor value out of range.
761
```

## Deep Dive

Error handling hasn't always been so straightforward. In the early days, developers often ignored errors, leading to the dreaded "undefined behavior." As programming evolved, so did the tools — you now have exceptions in many languages, but they’re still an old-school 'check-it-first' in the Arduino world due to hardware constraints and C++ roots.

In Arduino programming, you often see `if-else` statements for error handling. But there are alternatives: using the `assert` function to stop execution if a condition fails or designing fail-safes within your hardware setup itself.

When implementing error handling, consider the impact of stopping the program versus allowing it to continue with a default or safe state. There's a trade-off, and the right choice depends on the potential harm of interruptions versus incorrect operation.

## See Also

Brush up on error detection and handling with these:

- Arduino Language Reference: https://www.arduino.cc/reference/en/
- Embedded Artistry's deeper look into error handling: https://embeddedartistry.com/blog/2017/05/17/creating-a-circular-buffer-in-c-and-c/
- C++ Error Handling: https://en.cppreference.com/w/cpp/error/exception

This should give you the know-how and confidence to avoid the pitfalls of errors in your Arduino adventures.
