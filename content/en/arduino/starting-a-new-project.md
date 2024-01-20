---
title:                "Starting a new project"
html_title:           "Bash recipe: Starting a new project"
simple_title:         "Starting a new project"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why?

Starting a new project in Arduino is like setting up a fresh canvas for a masterpiece in coding. It's a programmer's way of creating a practical solution from scratch, molding innovative ideas, fixing problems, or simply learning new things.

## How To:

Let's start a basic Arduino UNO project - Blinking LED.

Here's the sketch you're gonna use:

```Arduino
void setup() {
   pinMode(LED_BUILTIN, OUTPUT); // Initializes digital pin LED_BUILTIN as an output.
}

void loop() {
   digitalWrite(LED_BUILTIN, HIGH); // Turns the LED on
   delay(1000); // Wait for a second
   digitalWrite(LED_BUILTIN, LOW); // Turns the LED off
   delay(1000); // Wait for a second
}
```
Run this sketch. You'll see the inbuilt LED on your Arduino UNO board turning on and off every second.

## Deep Dive

The project we just waded into roots back to the BASIC language. It was in the '60s when the idea of turning a pin HIGH and LOW to control devices was introduced and adopted in many programming systems, including Arduino.

If you are not comfortable with onboard LEDs, you can consider alternatives like using an external LED, a Relay, or even a Motor, and connecting it to other digital pins. Just remember to replace `LED_BUILTIN` with the pin number your device is connected to.

And yes, the delay function! It's not just your regular 'wait and do nothing' command. During this 'delay', Arduino interrupts are still checked, millis() and micros() values get increased, and Serial data can still be sent and received. 

## See Also

Dig more into Arduino projects? Make yourself at home through these: 

1. [Arduino Project Hub](http://create.arduino.cc/projecthub): A platform to share and get inspired by Arduino projects.
2. [Arduino Forum](https://forum.arduino.cc/): Troubleshoot your problems, discuss, and learn.
3. [Arduino Documentation](https://www.arduino.cc/reference/en/): The ultimate guide to all things Arduino.