---
date: 2024-01-25 02:59:49.433600-07:00
description: "Organizing code into functions means breaking up your code into reusable\
  \ chunks, each chunk doing a specific job. Programmers do it to make code easier\
  \ to\u2026"
lastmod: 2024-02-19 22:05:18.788639
model: gpt-4-1106-preview
summary: "Organizing code into functions means breaking up your code into reusable\
  \ chunks, each chunk doing a specific job. Programmers do it to make code easier\
  \ to\u2026"
title: Organizing code into functions
---

{{< edit_this_page >}}

## What & Why?
Organizing code into functions means breaking up your code into reusable chunks, each chunk doing a specific job. Programmers do it to make code easier to read, debug, and reuse. It's like sorting Legos into bins - it saves you from rummaging through a chaotic pile every time you want to build something.

## How to:
Imagine you want to blink an LED. Without functions, your `loop` is a messy jumble. With functions, it's neat. Here's how:

```Arduino
const int LED_PIN = 13;

void setup() {
  pinMode(LED_PIN, OUTPUT);
}

void loop() {
  blinkLED(500); // Blink the LED every 500ms
}

// Function to blink an LED
void blinkLED(int delayTime) {
  digitalWrite(LED_PIN, HIGH);
  delay(delayTime);
  digitalWrite(LED_PIN, LOW);
  delay(delayTime);
}
```

Sample output: Your LED is happily blinking away, and the code's purpose is clear at a glance.

## Deep Dive
Before functions, programming was a linear road trip; you saw every pothole from start to end. After functions, it's more like hopping flights - you skip to the important parts. Historically, subroutines (early functions) were a revolution in programming, letting coders avoid repeating themselves – that’s the DRY principle, Don’t Repeat Yourself. Alternatives to functions might include macros or the use of classes for object-oriented programming (OOP). The nitty-gritty? When you define a function, you're giving the compiler a blueprint for executing a task. With Arduino, you're often defining void functions that act as simple commands for a microcontroller, but functions can also return values, making them more versatile.

## See Also
For more on functions, browse through these:

- Arduino's official function reference: https://www.arduino.cc/reference/en/language/functions/
- Learn more about the DRY principle: https://en.wikipedia.org/wiki/Don%27t_repeat_yourself
- A refresher on the history of subroutines: https://en.wikipedia.org/wiki/Subroutine
