---
date: 2024-01-20 17:50:13.004065-07:00
description: 'How to: Arduino doesn''t have built-in string interpolation, but you
  can get similar results with `sprintf()` or by concatenating strings and variables.'
lastmod: '2024-03-13T22:45:00.309739-06:00'
model: gpt-4-1106-preview
summary: Arduino doesn't have built-in string interpolation, but you can get similar
  results with `sprintf()` or by concatenating strings and variables.
title: Interpolating a string
weight: 8
---

## How to:
Arduino doesn't have built-in string interpolation, but you can get similar results with `sprintf()` or by concatenating strings and variables.

```Arduino
char buffer[50]; // Make sure this is large enough to hold the final string
int sensorValue = analogRead(A0);
sprintf(buffer, "Sensor reading: %d", sensorValue);
Serial.println(buffer);
```

Output:
```
Sensor reading: 402
```

Or using string concatenation:

```Arduino
String message = "Sensor reading: " + String(sensorValue);
Serial.println(message);
```

## Deep Dive
C and C++ (the core languages of Arduino sketches) traditionally don't have string interpolation like newer languages (e.g., Python or JavaScript). Instead, `sprintf()` has been the go-to way to compose strings with variables. It works, but it can be a bit clunky and error-prone due to buffer overflows if not managed carefully.

Concatenation using the `String` class is more intuitive and safer from memory errors. The drawback? It can lead to memory fragmentation, especially in long-running programs on memory-constrained devices like Arduinos.

An alternative found in some newer or more specialized C++ libraries (not standard in Arduino) is to use string formatting libraries that provide a syntax closer to interpolation, such as `fmtlib`.

As for implementation details, when you concatenate with the `String` class, behind the scenes, the Arduino is creating new string objects and handling the memory for you. `sprintf()`, on the other hand, writes formatted text to a buffer you allocate, giving you more control at the cost of having to manage memory manually.

## See Also
- Arduino `String` class reference: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- `sprintf()` function reference: http://www.cplusplus.com/reference/cstdio/sprintf/
- Arduino Memory Optimization: https://www.arduino.cc/en/Tutorial/Foundations/Memory
- fmtlib, a modern string formatting library: https://fmt.dev/latest/index.html
