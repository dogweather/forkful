---
title:                "Arduino recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why
Debugging is an important part of programming, and the Arduino is no exception. Printing debug output allows you to track the execution of your code and easily identify any issues that may arise. This can save you time and effort in the long run, making it a valuable tool for any Arduino project.

## How To
Printing debug output on Arduino can be done using the `Serial.print()` or `Serial.println()` functions. These functions take in a string or variable as an argument and print it to the serial monitor, which can be accessed through the Arduino IDE. Let's take a look at an example:

```Arduino
int sensorValue = 0; // variable to store sensor reading

void setup() {
  // initialize serial communication
  Serial.begin(9600);
}

void loop() {
  // read sensor value
  sensorValue = analogRead(A0);
  
  // print sensor value to serial monitor
  Serial.print("Sensor value: ");
  Serial.println(sensorValue);
  
  // delay for 1 second
  delay(1000);
}
```

In this example, we are reading the analog value from pin A0 and printing it to the serial monitor every 1 second. This allows us to monitor the sensor value while the code is running and make any necessary adjustments.

## Deep Dive
There are a few things to keep in mind when printing debug output on Arduino. First, make sure to use the correct baud rate in the `Serial.begin()` function. This should match the baud rate selected in the serial monitor. Additionally, you can use different forms of the `Serial.print()` function, such as `Serial.print()` for printing without a new line or `Serial.println()` for printing with a new line. This can be useful when debugging different types of data or printing multiple values.

Another important aspect is managing the amount of output being printed. Too many debug statements can slow down your code and affect its performance. It's best to only include necessary output statements and remove them once you have identified and fixed any issues.

## See Also
For more information on printing debug output on Arduino, check out these resources:

- [Arduino Serial Communication](https://www.arduino.cc/en/Serial/Print)
- [Debugging Arduino Code](https://create.arduino.cc/projecthub/Arduino_Genuino/debugging-arduino-code-f6b42f)

In conclusion, printing debug output on Arduino is a valuable tool for tracking the execution of your code and identifying any issues. Use it wisely and happy debugging!