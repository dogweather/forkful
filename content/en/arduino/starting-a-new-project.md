---
title:                "Starting a new project"
date:                  2024-01-20T18:02:33.167744-07:00
model:                 gpt-4-1106-preview
simple_title:         "Starting a new project"

category:             "Arduino"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why?

Starting a new project on Arduino means initializing a fresh sketch, the canvas for your code. Programmers do this to breathe life into new gadgets, from blinking LEDs to robotics.

## How to:

```Arduino
// Create a simple Blink sketch to start off a new Arduino project

void setup() {
  pinMode(LED_BUILTIN, OUTPUT); // Set the built-in LED as an output
}

void loop() {
  digitalWrite(LED_BUILTIN, HIGH); // Turn the LED on
  delay(1000);                     // Wait for a second
  digitalWrite(LED_BUILTIN, LOW);  // Turn the LED off
  delay(1000);                     // Wait for another second
}
```

Connect your Arduino board, upload the sketch, and watch the built-in LED blink every second.

## Deep Dive

When you embark on a new Arduino project, you're treading in the footsteps of countless inventors and tinkerers. Arduino started in 2005 in Ivrea, Italy, as a tool for students without a background in electronics and programming. Since then, it's become a staple in DIY electronics, prototyping, and educational coding.

There are alternatives to starting a project from scratch. You can modify existing code or use libraries to add complex features without reinventing the wheel – but nothing beats the excitement of creating something uniquely yours.

The sketch starts with the `setup()` function, which runs once to set up your hardware, followed by the `loop()` function, which runs continuously, letting you control your project's behavior. Master the use and structure of these functions, and you're well on your way to becoming an Arduino pro.

## See Also

- Official Arduino Documentation: https://www.arduino.cc/reference/en/
- Intro to Arduino Sketches: https://www.arduino.cc/en/Tutorial/BuiltInExamples
- Arduino Forum – Project Guidance: https://forum.arduino.cc/c/project-guidance/8
