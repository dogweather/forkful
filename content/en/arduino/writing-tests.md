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

Writing tests in Arduino refers to the process of creating small programs to check the functionality of your main code. This is done to ensure that your code works as intended and to catch any potential bugs before they become a bigger problem. Programmers do it to improve the overall quality and reliability of their code, making sure it performs correctly in different scenarios.

## How to:

To write tests in Arduino, you can use a framework called "ArduinoUnit". This makes it easy to create and run tests directly on your Arduino board. Check out the following example code:

```Arduino
#include <ArduinoUnit.h>

test(helloWorld) {
  Assert.assertEquals("Hello World!", "Hello World!");
}

void setup() {
  test::run();
}

void loop() {}
```

In this code, we first include the necessary library and then use the ```test()``` function to define a test called "helloWorld". We then use the ```Assert.assertEquals()``` function to check if the expected result matches the actual result. Finally, we use the ```test::run()``` function to run our test. You can add as many tests as you need and even test specific functions or variables within your main code.

## Deep Dive:

In the early days of Arduino, writing tests was not a common practice. However, as more and more complex projects were developed, the need for a reliable way to test code also increased. This led to the creation of ArduinoUnit, which now comes pre-installed with the Arduino IDE. Other popular alternatives include "Unity" and "CppUTest".

To implement tests in your Arduino projects, you first need to understand the structure of a test. A test consists of three parts: a setup, the code to be tested, and an assertion. The setup prepares the environment for the test, the code to be tested is the part of your main code that you want to test, and the assertion checks if the expected result matches the actual result.

## See Also:

- ArduinoUnit: https://github.com/mmurdoch/arduinounit
- Unity: https://github.com/ThrowTheSwitch/Unity
- CppUTest: https://github.com/cpputest/cpputest