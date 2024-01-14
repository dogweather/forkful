---
title:    "Arduino recipe: Reading command line arguments"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why 

As an Arduino programmer, it can often be helpful to have your program take in inputs from the command line. This allows for more flexibility and customization in your code.

## How To

In order to read command line arguments in Arduino, you will need to utilize the `Serial` and `String` functions. First, set up the serial communication by including the `Serial.begin()` function in the `setup()` section of your code. Then, you can use the `Serial.readString()` function to read in the arguments as a string. Next, you can use `Serial.parseFloat()` or `Serial.parseInt()` to convert the string into a float or integer, respectively. Finally, use the `Serial.print()` or `Serial.println()` functions to output the argument values.

```Arduino
void setup() {
  Serial.begin(9600); // initialize serial communication
}

void loop() {
  String input = Serial.readString(); // read in the arguments as a string
  float num = Serial.parseFloat(); // convert string into float
  int num2 = Serial.parseInt(); // convert string into integer

  Serial.print("The first argument is: ");
  Serial.println(input); // output the string argument
  Serial.print("The second argument as a float: ");
  Serial.println(num); // output the float argument
  Serial.print("The third argument as an integer: ");
  Serial.println(num2); // output the integer argument
}
```

## Deep Dive

Reading command line arguments can be extremely useful for creating dynamic and customizable programs. In addition to the methods mentioned above, the `Serial.readStringUntil()` function can also be used to read in a string until a specific character is encountered. For example, `Serial.readStringUntil(',')` would read in a string until a comma is reached. This makes it useful for handling multiple arguments separated by a specific character.

However, it's important to note that command line arguments can only be read in one at a time. In order to read in multiple arguments at once, they must be entered in order and separated by the default Arduino serial monitor line ending, which is a carriage return or newline character.

## See Also

If you want to learn more about command line arguments and other ways to handle user input in Arduino, check out these helpful resources:

- [Arduino Command Line Interface (CLI)](https://www.arduino.cc/pro/cli)
- [Arduino Serial Communication](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [Parsing and Formatting Strings with Arduino](https://www.robotshop.com/community/tutorials/show/arduino-parsing-string-data-from-serial-input)