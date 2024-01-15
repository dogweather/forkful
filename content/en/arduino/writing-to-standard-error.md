---
title:                "Writing to standard error"
html_title:           "Arduino recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why

Writing to the standard error in Arduino can be useful for debugging and error handling in your program. It allows you to display error messages or other important information to the serial monitor, making it easier to identify and fix any issues in your code.

## How To

To write to the standard error in Arduino, you can use the `Serial.println()` function. This function takes in a string or variable and sends it to the standard error, which can be viewed in the serial monitor.

For example, if you wanted to display an error message, you could write:

```Arduino
Serial.println("Error: something went wrong");
```

This will print the message "Error: something went wrong" in the serial monitor.

You can also use this function to display variable values for debugging purposes. For instance, if you have a variable `temperature` that holds the temperature reading from a sensor, you could write:

```Arduino
Serial.println("Current temperature: " + String(temperature));
```

This will print "Current temperature: [temperature value]" in the serial monitor.

## Deep Dive

The standard error in Arduino is a stream that is separate from the standard output. This means that you can write to the standard error without affecting the standard output and vice versa. This is useful for differentiating between normal program output and error messages.

You can also use the `Serial.print()` function to write to the standard error. However, this function does not add a new line at the end like `Serial.println()` does. So if you want to go to a new line, you will need to add the `\n` character at the end of your string.

For example:

```Arduino
Serial.print("Some important info");
Serial.println("This will be on a new line");
```

This will output:

```
Some important infoThis will be on a new line
```

To fix this, you can write:

```Arduino
Serial.print("Some important info\n");
Serial.println("This will be on a new line");
```

This will output:

```
Some important info
This will be on a new line
```

## See Also

- [Arduino Reference - Serial](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [Serial.println vs Serial.print](https://www.arduino.cc/reference/en/language/functions/communication/serial/print/)
- [Debugging Arduino Code using Serial Print](https://create.arduino.cc/projecthub/CircuitDigest/debugging-arduino-code-using-serial-print-debug-mode-8a1b92)