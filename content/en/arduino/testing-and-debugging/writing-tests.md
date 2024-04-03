---
date: 2024-02-03 19:03:20.256456-07:00
description: "How to: Arduino does not have a built-in testing framework like some\
  \ other programming environments. However, you can use third-party libraries such\
  \ as\u2026"
lastmod: '2024-03-13T22:45:00.324402-06:00'
model: gpt-4-0125-preview
summary: Arduino does not have a built-in testing framework like some other programming
  environments.
title: Writing tests
weight: 36
---

## How to:
Arduino does not have a built-in testing framework like some other programming environments. However, you can use third-party libraries such as `AUnit` for unit testing Arduino code. AUnit is inspired by Arduino's built-in library, `ArduinoUnit`, and Google's testing framework, `Google Test`.

### Example with AUnit:
First, install AUnit via the Library Manager in the Arduino IDE: go to Sketch > Include Library > Manage Libraries... > search for AUnit and install it.

Then, you can write tests like so:

```cpp
#include <AUnit.h>

test(ledPinHigh) {
  const int ledPin = 13;
  pinMode(ledPin, OUTPUT);
  digitalWrite(ledPin, HIGH);
  assertTrue(digitalRead(ledPin));
}

test(ledPinLow) {
  const int ledPin = 13;
  pinMode(ledPin, OUTPUT);
  digitalWrite(ledPin, LOW);
  assertFalse(digitalRead(ledPin));
}

void setup() {
  Serial.begin(9600);
  aunit::TestRunner::run();
}

void loop() {
  // Empty
}
```
After uploading this test to your Arduino board, open the Serial Monitor to view the test results. You should see output indicating whether each test passed or failed:

```
TestRunner started on 2 test(s).
Test ledPinHigh passed.
Test ledPinLow passed.
TestRunner duration: 0.002 seconds.
TestRunner summary: 2 passed, 0 failed, 0 skipped, 0 timed out, out of 2 test(s).
```

This simple example demonstrates using AUnit for testing the state of an LED pin. By creating tests, you confirm that your Arduino behaves as expected in different conditions. With AUnit, you can write more complex tests, test suites, and enjoy features like test timeouts and setup/teardown procedures for more advanced scenarios.
