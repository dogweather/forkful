---
title:                "编写测试"
html_title:           "Arduino: 编写测试"
simple_title:         "编写测试"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/writing-tests.md"
---

{{< edit_this_page >}}

# Test Writing in Arduino

## What & Why?
Writing tests in Arduino is the process of creating test cases to check the functionality and performance of your code. Programmers do this to ensure that their code functions as intended and to catch any potential bugs or errors before they become bigger problems.

## How to:
To write tests in Arduino, you can use the built-in library called "ArduinoUnit". This library allows you to create test cases and compare the expected and actual results.

```
ArduinoUnit test;

test.assertEqual(5, 2+3);  // This test will pass
test.assertNotEqual(8, 2*3); // This test will fail
test.assertTrue(6 > 4 || 6 < 3); // This test will pass
test.assertFalse(2 < 1); // This test will fail

test.done();  // This line marks the end of the test code
```

## Deep Dive:
Writing tests has become an essential practice in modern programming, especially in the context of agile development. It helps developers to catch bugs early on and ensures that code changes do not lead to unintended consequences. Before the establishment of formal testing frameworks, developers used manual testing methods, which were time-consuming and prone to errors.

Other alternatives to ArduinoUnit include Unity, Google Test, and Catch. These libraries offer more complex and advanced features, but they require additional setup and may not be suitable for small projects.

To implement test writing in your Arduino code, you will need to have a thorough understanding of the programming language and the Arduino platform. It is also essential to have a well-defined testing strategy and to follow best practices to ensure accurate and efficient testing.

## See Also:
- [ArduinoUnit Library](https://github.com/mmurdoch/arduinounit)
- [Introduction to Test Driven Development in Arduino](https://blog.arduino.cc/2019/08/16/introduction-to-test-driven-development-in-arduino/)
- [Test Writing in Arduino tutorial](https://canvas.okstate.edu/courses/41515/files/2237283/preview?verifier=edBMdfEHK7g9X98I1G2fxNVFN91GmiGjo37g04p2)