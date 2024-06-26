---
date: 2024-01-25 03:00:01.439059-07:00
description: 'How to: In Arduino, you can round numbers using built-in functions.
  Key players are `round`, `ceil`, and `floor`. Here''s a quick demo.'
lastmod: '2024-03-13T22:45:00.317538-06:00'
model: gpt-4-1106-preview
summary: In Arduino, you can round numbers using built-in functions.
title: Rounding numbers
weight: 13
---

## How to:
In Arduino, you can round numbers using built-in functions. Key players are `round`, `ceil`, and `floor`. Here's a quick demo:

```arduino
void setup() {
  Serial.begin(9600);
  
  float myNumber = 123.4567;

  // Round to the nearest whole number
  Serial.println(round(myNumber)); // Outputs: 123

  // Always rounds up
  Serial.println(ceil(myNumber));  // Outputs: 124

  // Always rounds down
  Serial.println(floor(myNumber)); // Outputs: 123
}

void loop() {
  // Nothing to loop through.
}
```

## Deep Dive:
Rounding algorithms have a long history; they've been around long before digital computers. In analog computing, rounding was a physical process. In digital computing, it's a mathematical one.

Rounding is needed when we convert from a type with more precision (like `float` or `double`) to a type with less precision (like `int`). But how we round can vary:

1. `round()`: Standard rounding. If the fraction is 0.5 or higher, it goes up; else, it goes down.
2. `ceil()`: Short for "ceiling", always rounds up to the nearest whole number, even if it's closer to the lower number.
3. `floor()`: Opposite of ceiling; always rounds down.

Choosing between these functions hinges on what the rounded value is for. Measurements might need standard rounding, money often uses `floor`, while inventory systems might use `ceil` to ensure everything's accounted for.

Arduino's implementation of these functions is straightforward; they don't handle extra cases like rounding to specific decimal places. For that, a custom function or deeper mathematics comes into play—think of multiplying to shift the decimal, rounding, then dividing back.

Round-off errors can accumulate, significantly impacting long calculations or iterative processes. Programmers need to be cautious when running numerous operations on rounded values.

## See Also:
2. In-depth look at the pitfalls and strategies for rounding: [Floating Point Guide](https://floating-point-gui.de/)
3. For advanced techniques, including custom rounding functions and handling round-off error, you might check academic resources or detailed programming guides.
