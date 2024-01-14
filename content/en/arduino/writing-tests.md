---
title:                "Arduino recipe: Writing tests"
simple_title:         "Writing tests"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/writing-tests.md"
---

{{< edit_this_page >}}

## Why

When it comes to programming, testing is a crucial aspect that is often overlooked. Writing tests allows you to catch bugs and errors in your code before they become larger problems. It also helps to ensure that your code is functioning as intended, making it easier to make any necessary changes or updates in the future.

## How To

Writing tests for your Arduino programs is a simple process that can save you a lot of time and frustration in the long run. Here is an example of how you can write tests for an LED blink program:

```Arduino
// Include the necessary libraries
#include <Arduino.h>
#include "unity.h"

// Set up variables for testing
const int ledPin = 13;

// Set up the test function
void test_ledBlink() {
  digitalWrite(ledPin, HIGH);    // Turn on the LED
  delay(1000);                   // Wait for 1 second
  digitalWrite(ledPin, LOW);     // Turn off the LED
  delay(1000);                   // Wait for 1 second
}

// Set up the setup function
void setUp() {
  pinMode(ledPin, OUTPUT);       // Set the LED pin as an output
}

// Set up the teardown function
void tearDown() {
  pinMode(ledPin, INPUT);       // Reset the LED pin back to an input
}

// Set up the main function
int main() {
  UNITY_BEGIN();       // Start the unit testing framework
  RUN_TEST(test_ledBlink);      // Run the test function
  UNITY_END();         // Finish the unit testing framework
}
```

The above code will turn an LED connected to pin 13 on for 1 second, then off for 1 second, and continue to repeat. This is a simple test to ensure that the LED is functioning properly.

To run this test, you will need to install the Unity testing framework on your Arduino IDE. You can find instructions on how to do this here: [https://github.com/ThrowTheSwitch/Unity/wiki/Using-Unity-with-Arduino](https://github.com/ThrowTheSwitch/Unity/wiki/Using-Unity-with-Arduino).

Once you have installed Unity, you can simply click on the "Verify" button in your IDE to run the test. If everything is working properly, you should see a "Test Passed" message in the output window.

## Deep Dive

Writing tests for your Arduino programs is not only about checking if your code works, but it's also about making sure that it remains functional as you make changes to it. This is where unit testing comes into play.

Unit testing involves breaking your code down into smaller, testable units, and checking each unit individually. This allows you to identify and fix any problems with the code before they spread to other parts of your program.

Another important aspect of testing is using code coverage. This is a metric that measures how much of your code is actually being tested. The higher the code coverage, the more confident you can be that your code is functioning properly.

There are various libraries and frameworks available for Arduino that can help you with unit testing and code coverage, such as Unity, ArduinoUnit, and PlatformIO. It's important to do some research and find the one that best fits your needs.

## See Also

- [https://www.arduino.cc/en/Guide/Environment](https://www.arduino.cc/en/Guide/Environment)
- [https://learn.adafruit.com/testing-and-maintaining-your-arduino-sketch/code-coverage](https://learn.adafruit.com/testing-and-maintaining-your-arduino-sketch/code-coverage)
- [https://www.hackster.io/arrisary/tdd-in-arduino-f70f0e](https://www.hackster.io/arrisary/tdd-in-arduino-f70f0e)