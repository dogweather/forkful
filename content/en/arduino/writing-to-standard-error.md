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

## What & Why?

Writing to standard error is a way for programmers to communicate important information or error messages to the user during the runtime of a program. This allows for easier troubleshooting and debugging, as it provides real-time feedback on the status of the program.

## How to:

To write to standard error in Arduino, you can use the ```Serial``` object and the ```println()``` function. Simply connect your computer to your Arduino board and open the Serial Monitor. Then, use the ```Serial.println()``` function to print your desired message to the Serial Monitor. Here is a simple example:

```
void setup() {
  Serial.begin(9600); // initialize Serial communication at 9600 bits per second
}

void loop() {
  int myNumber = 10;
  Serial.println("My number is: "); // prints "My number is: " to the Serial Monitor
  Serial.println(myNumber); // prints the value of myNumber to the Serial Monitor
  delay(1000); // waits for 1 second before repeating
}
```

The output in the Serial Monitor will look like this:

```
My number is: 
10
```

## Deep Dive:

Writing to standard error has been a common practice among programmers for a long time. In older programming languages, such as C and C++, printing to standard error was the only way to display information during program execution. However, with the advancement of programming languages and development tools, new methods such as logging and debugging have also become popular.

When using ```Serial.println()```, keep in mind that the baud rate (bits per second) needs to match the one set in the Serial Monitor. Otherwise, the output will be garbled or nonexistent.

## See Also:

For more information on writing to standard error in Arduino, you can refer to the official Arduino documentation: https://www.arduino.cc/reference/en/language/functions/communication/serial/println/

If you want to learn more about logging and debugging in Arduino, check out this informative tutorial: https://randomnerdtutorials.com/guide-to-arduino-debugging-methods/