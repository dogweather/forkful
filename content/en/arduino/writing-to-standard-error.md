---
title:                "Arduino recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why

Writing to standard error is an essential skill for Arduino programmers. It allows you to easily debug and troubleshoot your code, helping you identify and fix errors quickly. By understanding how to write to standard error, you can improve the efficiency and accuracy of your code.

## How To

To write to standard error in Arduino, you will need to use the `Serial.println()` function. This function takes in a string or variable as an argument and prints it to the serial monitor. Here is an example code:

```Arduino
int num = 5;

Serial.println("The value of num is: ");
Serial.println(num);
```

The output of this code will be:

```
The value of num is:
 5
```

As you can see, the variable `num` is printed to the serial monitor. This is the most basic way of writing to standard error. You can also use `Serial.print()` to print without a new line, or use concatenation to print multiple values in one line. Here is an example:

```Arduino
int a = 10;
int b = 5;

Serial.println("The sum of a and b is: " + a + b);
```

The output of this code will be:

```
The sum of a and b is: 15
```

Another helpful technique is to use `Serial.write()` to print non-string data types, such as integers, as bytes instead of converting them to text.

## Deep Dive

To truly understand writing to standard error, it's important to know the difference between `Serial.print()` and `Serial.println()`. The former prints the specified data without a new line, while the latter prints with a new line character at the end.

Additionally, understanding serial communication is crucial in learning how to write to standard error. The serial monitor is essentially a virtual connection between your computer and the Arduino board, allowing you to send and receive data. Writing to standard error helps you track the data being sent from your Arduino board to the serial monitor.

## See Also

For more information on writing to standard error, check out these resources:

- [Arduino Serial Communication](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [Arduino Serial.println()](https://www.arduino.cc/reference/en/language/functions/communication/serial/println/)
- [Arduino Serial.print()](https://www.arduino.cc/reference/en/language/functions/communication/serial/print/)
- [Serial Communication in Arduino](https://maker.pro/arduino/tutorial/arduino-serial-communication-everything-you-need-to-know)