---
title:                "Writing tests"
html_title:           "Arduino recipe: Writing tests"
simple_title:         "Writing tests"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
Writing tests means creating a set of conditions to check if your code behaves as expected. Programmers do it to catch bugs early, ensure software quality, and save the headache when adding new features.

## How to:
```Arduino
#include <Arduino.h>
#include <unity.h>

void setUp(void) {
// set stuff up here
}

void tearDown(void) {
// clean up here
}

void test_led_builtin_pin_number(void) {
    TEST_ASSERT_EQUAL(13, LED_BUILTIN);
}

void test_led_state_high(void) {
    digitalWrite(LED_BUILTIN, HIGH);
    TEST_ASSERT_EQUAL(digitalRead(LED_BUILTIN), HIGH);
}

void setup() {
    UNITY_BEGIN();
    RUN_TEST(test_led_builtin_pin_number);
    RUN_TEST(test_led_state_high);
    UNITY_END();
}

void loop() {
    // Usually empty in testing
}
```
Output:
```
.
.
OK
```

## Deep Dive
Historical context: Testing in Arduino came later than in software dev and was less common due to hardware interaction. Alternatives: Manual testing, or more complex test frameworks like Google Test. Implementation details: Typically we use a library like ArduinoUnit or AUnit. Place tests in `setup()` and keep `loop()` empty as tests run once.

## See Also
- ArduinoUnit library: https://github.com/mmurdoch/arduinounit
- AUnit library: https://github.com/bxparks/AUnit
- Introduction to Unit Testing: https://www.arduino.cc/en/Guide/UnitTesting