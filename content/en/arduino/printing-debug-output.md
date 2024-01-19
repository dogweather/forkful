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

# Print Debug Output in Arduino

## What & Why?

Printing debug output is the process of sending informational messages from your program running on the Arduino board to your PC. It's crucial for figuring out what's happening inside your program, particularly when it is misbehaving.

## How to:

Using Arduino's built-in function `Serial.print()` and its sibling `Serial.println()`, we can easily print debug output. Let's walk through some code.

```Arduino
void setup() {
  Serial.begin(9600); // Setting up serial communication at 9600 bps
}

void loop() {
  int sensorValue = analogRead(A0); // Reading the sensor value
  Serial.print("Sensor Value: "); // Printing the text
  Serial.println(sensorValue); // Printing the sensor value
  delay(200); // Waiting a bit before running the loop again
}
```

In the serial monitor, you would see output like this:

```Arduino
Sensor Value: 325
Sensor Value: 326
Sensor Value: 327
```

## Deep Dive

`Serial.print()` originated from C++ and is one of the staple functions in Arduino programming for sending data back to a computer via the USB connection.

Plenty of alternatives exist for `Serial.print()`. `sprintf()` and `snprintf()` are two functions you can use to format your debug output in more complicated ways. They allow including variable values within strings, a common requirement. 

An important detail to bear in mind: `Serial.print()` does not have an automatic line feed. If you want your subsequent output on a new line, either use `Serial.println()` or include `\n` at the end of your string.

## See Also

- If you want to get a deeper understanding of `Serial.print()`, the Arduino reference is a great place to start: [Arduino - Serial](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- For a detailed use-case example: [Reading Serial data from an Arduino using C#](https://maker.pro/arduino/tutorial/how-to-send-serial-data-from-an-arduino-to-a-c-sharp-program)
- If you want to dig into alternatives like `sprintf()`, check out: [How to Use sprintf() on Arduino](https://www.baldengineer.com/arduino-how-to-use-sprintf.html)