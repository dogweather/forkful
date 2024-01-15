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

## Why
Writing tests for your Arduino code is a crucial step in ensuring the reliability and functionality of your projects. By following a test-driven development approach, you can catch bugs and errors early on and save yourself from headaches down the line.

## How To
First, we need to set up our testing environment. We will be using the Arduino Uno board for our examples, but this approach can be applied to other Arduino boards as well. 
```Arduino
void setup() {
  //initialize code for testing here
}

void loop() {
  //main program code
}
```
Next, we need a testing framework. One popular option is the Arduino Unit Testing Library (https://github.com/mmurdoch/arduinounit). This library allows us to write and run automated tests for our code. 
```Arduino
#include <ArduinoUnit.h> //include the testing library

//sample test case
unittest(myTestCase) {
  assertTrue(1 < 2); //test for a condition to be true
}

void setup() {
  Test::run(); //runs all the defined test cases
}

void loop() {
  //main program code
}
```
After writing our tests, we need to upload our code to the Arduino board and open the serial monitor to view the test results. The output should look like this:
```
OK (1 test, 1 ran, 1 assertions, 0 failures, 0 errors, 0 skipped, 0 todo)
```
If all of our tests pass, we will see a green "OK" and the number of tests that ran. Otherwise, our failing tests will be highlighted in red.

## Deep Dive
One key aspect of writing tests is knowing what to test. So how do we determine what to test in our Arduino code? Here are some tips to keep in mind:
- Test all inputs and outputs: This includes testing sensor readings, button presses, and servo movements.
- Test edge cases: These are situations where input values could potentially cause errors in our code.
- Test for expected behavior: Make sure your code is producing the desired output.
- Test for failure: Don't just test for successful outcomes, also test for potential failures and how your code handles them.

Another important factor in writing tests is organization. Keeping our test cases organized not only makes our code easier to read but also helps us pinpoint the source of errors when they occur. This can be achieved by using descriptive test case names and grouping related tests together.

## See Also
- Arduino Unit Testing Library: https://github.com/mmurdoch/arduinounit
- Test-Driven Development for Arduino: https://www.arduino.cc/en/Guide/PaperTest
- Writing High-Quality Tests for Arduino: https://www.programmableweb.com/news/writing-high-quality-tests-arduino/how-to/2016/10/12