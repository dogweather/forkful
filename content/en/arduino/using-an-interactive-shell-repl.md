---
title:                "Using an interactive shell (REPL)"
date:                  2024-01-25T03:39:53.083122-07:00
model:                 gpt-4-1106-preview
simple_title:         "Using an interactive shell (REPL)"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## What & Why?
An interactive shell, or REPL (Read-Eval-Print Loop), lets you write and run code piece by piece - perfect for quick tests and playing with ideas. Programmers use it for debugging, learning, or fine-tuning their code without the hassle of compiling full programs.

## How to:
```Arduino
#include <Arduino.h>

void setup() {
  Serial.begin(115200); 
}

void loop() {
  if (Serial.available() > 0) {
    String input = Serial.readStringUntil('\n');
    Serial.println("You typed: " + input);
    // Here, you can add eval code to run commands
  }
}
```
Sample Output:
```
> led on
You typed: led on
> print 123
You typed: print 123
```

## Deep Dive
REPL isn't built-in in Arduino's default IDE, but creative programmers can simulate one using serial communication. Historically, Lisp was one of the first languages offering a REPL, emphasizing interactive development. Today, alternatives like Python boast powerful shells (like IPython). While an Arduino REPL lacks the sophistication found in these environments, it can still provide real-time feedback and control, especially when combined with an interpreter library like Bitlash or an external tool such as PlatformIO's Monitor, which supports basic REPL and scripting features.

## See Also
- [Arduino Serial Documentation](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [Interactive Development Environment (IDE) Comparison](https://en.wikipedia.org/wiki/Comparison_of_integrated_development_environments)
