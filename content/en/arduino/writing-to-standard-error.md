---
title:                "Arduino recipe: Writing to standard error"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why

Writing to standard error may seem like an insignificant task, but it can actually be quite useful for debugging and error handling in Arduino programming. By printing messages to standard error, you can easily keep track of what's happening in your code and identify any issues that may arise.

## How To

To write to standard error in Arduino, you can use the `Serial` object and the `Serial.println()` function. Here's an example code block that shows how to print a message to standard error:

```Arduino
Serial.println("This is an error message.");
```

By default, the message will be printed to the serial monitor. However, you can also redirect the output to a different device, such as a computer terminal, by using the `Serial.begin()` function with a specific baud rate.

Once you've redirected the output, you can easily view any error messages that are printed during the execution of your code. This can be especially useful if you're working with complex logic or multiple sensors and want to keep track of their behavior.

## Deep Dive

In addition to debugging and error handling, writing to standard error can also be helpful for logging data in your Arduino projects. By printing information to standard error, you can create a simple log file that can be accessed later for analysis or troubleshooting.

It's important to note that standard error is different from standard output, which is what is typically used for printing messages in Arduino. Standard error is specifically designed for displaying error messages and should not be used for regular program output.

## See Also

- [Serial.println() reference](https://www.arduino.cc/reference/en/language/functions/communication/serial/println/)
- [Redirect Arduino output to serial monitor or other device](https://www.impulseadventure.com/elefootprint/arduino-serial-output-fmt.html)
- [Using the Standard Library in Arduino](https://www.arduino.cc/reference/en/#structure)
- [Arduino Serial Communication Basics](https://embedded.fm/blog/2016/3/2/arduino-serial-communication-basics)