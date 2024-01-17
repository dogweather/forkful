---
title:                "Printing debug output"
html_title:           "Arduino recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?
When programming with Arduino, you may come across the term "printing debug output". This simply means sending messages or data to be displayed on the serial monitor via USB. Programmers do this to gather information about the code's execution, spot errors, and troubleshoot issues.

## How to:
To print debug output in Arduino, you can use the ```Serial.print()``` function. This function takes in parameters for the information you want to display. For example, if you want to print the value of a variable named "sensorValue", you would use the following code:

```Arduino
Serial.print(sensorValue);
```
This will send the value of the variable to the serial monitor, which you can view by clicking on the magnifying glass icon at the top right of the Arduino IDE.

You can also use the ```Serial.println()``` function to print a line break after the information. For example:

```Arduino
Serial.println(sensorValue);
```

## Deep Dive:
Historically, printing debug output was done through physical output devices such as LED lights and displays. However, with the use of USB and the serial monitor, it has become much easier for programmers to gather and analyze data during the code's execution.

An alternative to printing debug output is using breakpoints, which allow you to pause the code at a specific point and check the value of variables. This can be useful, but requires more effort and can only be done at specific points in the code.

To print debug output, the Serial library in Arduino must be initialized with the ```Serial.begin()``` function. This sets up the communication between the Arduino board and the serial monitor.

## See Also:
- [Arduino Serial.print() documentation](https://www.arduino.cc/reference/en/language/functions/communication/serial/print/)
- [Arduino Serial.println() documentation](https://www.arduino.cc/reference/en/language/functions/communication/serial/println/)
- [Tutorial: Debugging Arduino Code with Serial Printing](https://www.arduino.cc/en/Tutorial/SerialDebugging)
- [Using Breakpoints in Arduino IDE](https://www.arduino.cc/en/Tutorial/BreakoutDetection)