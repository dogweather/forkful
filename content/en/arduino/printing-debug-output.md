---
title:    "Arduino recipe: Printing debug output"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why

If you're an Arduino enthusiast, you know that debugging can be a crucial part of the programming process. Printing debug output allows you to track the flow of your code and identify any errors or issues that may arise. It can also help you better understand how your program is functioning and make necessary adjustments. So why not make use of this useful feature in your Arduino projects?

## How To

To print debug output in Arduino, you can use the `Serial` object and its associated functions. This allows you to send data from your Arduino board to your computer via the USB connection. Here's an example of how you can use it in your code:

```arduino
void setup() {
  // initialize serial communication at a baud rate of 9600
  Serial.begin(9600);
}

void loop() {
  // print a string to the serial monitor
  Serial.println("Hello world!");
  
  // print an integer variable
  int x = 5;
  Serial.println("The value of x is: ");
  Serial.println(x);
  
  // print a floating point number with two decimal places
  float pi = 3.14;
  Serial.print("The value of pi is: ");
  Serial.println(pi, 2);
  
  // print multiple variables in one statement
  Serial.print("The value of x is: ");
  Serial.print(x);
  Serial.print(", and the value of pi is: ");
  Serial.println(pi);
}

```

After uploading this code to your Arduino board, open the Serial Monitor in the Arduino IDE. You should see the output data displayed on the screen, corresponding to the `Serial` function calls in your code.

```
Hello world!
The value of x is: 
5
The value of pi is: 3.14
The value of x is: 5, and the value of pi is: 3.14
```

## Deep Dive

The `Serial` object has several other functions that you can use for printing debug output, such as `Serial.write()` for sending binary data and `Serial.print()` for formatting the output in different ways (decimal, hexadecimal, binary, etc.). You can also use the `Serial.read()` function to read data from the serial port.

Additionally, you can use `Serial.begin()` to set a specific baud rate for communication, `Serial.available()` to check if there is any data available to be read, and `Serial.setTimeout()` to specify a timeout period for reading data.

Overall, the `Serial` object provides a versatile and easy-to-use way to print debug output in your Arduino projects.

## See Also

- [Arduino Documentation on Serial](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [Arduino Playground - Serial Communication](https://playground.arduino.cc/Interfacing/CPPWindows/)