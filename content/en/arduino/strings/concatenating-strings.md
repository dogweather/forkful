---
date: 2024-01-20 17:33:50.055851-07:00
description: "How to: Let's do some string-joining! All within setup because we just\
  \ want a quick look\u2014no need for a repeat loop."
lastmod: '2024-04-05T22:37:43.278467-06:00'
model: gpt-4-1106-preview
summary: Let's do some string-joining!.
title: Concatenating strings
weight: 3
---

## How to:
Let's do some string-joining! All within setup because we just want a quick look—no need for a repeat loop.

```arduino
void setup() {
  // Start serial communication
  Serial.begin(9600);

  // Create two strings
  String greeting = "Hello, ";
  String name = "Arduino!";

  // Concatenate them
  String combined = greeting + name;

  // Print the result
  Serial.println(combined); 
}
void loop() {
  // Nothing to loop over here
}
```

You run it, and the output waits for you in the Serial Monitor:

```
Hello, Arduino!
```

## Deep Dive
Concatenating strings is old as hills in programming—been around since early languages took their baby steps. In Arduino, you can use either the `+` operator like we did, or the `+=` to tack a string onto an existing one. Behind the scenes, these operators are actually calling functions that handle memory allocation and copying the characters efficiently.

Why not always concatenate? Well, if you're dealing with tiny microcontrollers and doing a lot of string-merging, you could run into memory issues—because every time you combine, you create a new string, consuming more memory. For heavy string manipulation, folks sometimes resort to character arrays (classic C-style) to save space and avoid potential performance hits.

Also, check string functions like `concat()`, which can add not just strings but other data types to an existing string.

## See Also
Looking for more? Here’s where to dive deeper:
- Arduino String Reference: [arduino.cc/reference/en/language/variables/data-types/string/](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- Memory Management in Arduino: [learn.adafruit.com/memories-of-an-arduino](https://learn.adafruit.com/memories-of-an-arduino)
- The Evils of Arduino Strings: [majenko.co.uk/blog/evils-arduino-strings](https://majenko.co.uk/blog/evils-arduino-strings)
