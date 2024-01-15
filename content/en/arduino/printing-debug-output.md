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

## Why

Debugging is an essential part of programming, and one of the most effective ways to debug code is by printing out relevant information during runtime. This allows you to track the flow of your code and identify any potential issues or bugs. In this article, we will explore how to use the "Serial" library in Arduino to print debug output.

## How To

To use the "Serial" library, you will first need to open the Serial Monitor in the Arduino IDE. This can be done by clicking on the "Tools" menu and then selecting "Serial Monitor". 

Once the Serial Monitor is open, you can use the "Serial.println()" function to print out a specific value or variable. For example, ```Arduino
Serial.println("Hello, world!"); 
``` 
will print out "Hello, world!" in the Serial Monitor.

You can also use the "Serial.print()" function to print without adding a line break. This can be useful when you want to print out multiple values on the same line. For example, ```Arduino
int x = 10;
Serial.print("The value of x is: ");
Serial.print(x);
``` 
will print out "The value of x is: 10" on the same line.

Another useful function is "Serial.begin()" which is used to initialize the communication between your Arduino board and the Serial Monitor. This function should be called in the "setup" section of your code. For example, ```Arduino
void setup() {
  Serial.begin(9600); //sets the baud rate to 9600
}
``` 

## Deep Dive

The "Serial" library offers even more functions that can be useful for debugging. For example, "Serial.read()" can be used to read input from the Serial Monitor, and "Serial.available()" can check for any incoming data. You can also use "Serial.setTimeout()" to set a timeout period for "Serial.read()" where if no data is received within the specified time, the function will return -1.

To make your debug output more organized, you can also use different formatting options with the "println" and "print" functions. For example, using "\t" will add a tab space and "\n" will add a new line.

It is important to note that using the "Serial" library for debugging can slow down your code's execution speed. To avoid this, you can use conditional statements to check if the Serial Monitor is connected before printing any debug output. For example, ```Arduino
if(Serial) {
  Serial.println("Debug output here");
}
``` 

## See Also

- [Arduino Documentation on Serial](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [Tutorial on Debugging with Serial Communication](https://www.instructables.com/Arduino-Debugging-with-Serial-Communication/)
- [Tips for Effective Debugging in Arduino](https://www.makeuseof.com/tag/arduino-debugging-tips/)