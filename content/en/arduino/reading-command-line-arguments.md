---
title:                "Reading command line arguments"
html_title:           "C++ recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?
Command line arguments are parameters fed to a program at the moment it starts, controlling its behavior from the get-go. Programmers use command line arguments to make programs flexible and customizable for different use cases without the need for recompiling.

## How To:
Let's dive straight into it. Since Arduino does not natively support command line parameters, we use Serial communication. Here's a generalized code to read arguments from Serial Monitor:

```Arduino
String content = "";

void setup() {
  Serial.begin(9600);
  Serial.setTimeout(100);
}

void loop() {
  if (Serial.available()) {
    content = Serial.readStringUntil('\n');
    handleContent();
    content = "";
  }
}

void handleContent() {
  //Parse and interpret content here.
}
```

In this example, the content string holds the data that comes from the serial port, which can be used as command-line arguments. The \n character signifies the end of input. `handleContent()` is where you can parse and use this content.

Please modify the code to follow your project requirements.

## Deep Dive
Historically, command line arguments have their roots in Unix-like systems. They allow users to modify the behavior of the program at runtime.

In Arduino, alternatives to serial include using Control Board buttons or an LCD Keypad, or even wireless connectivity options depending on your project's requirements.

The Serial communication method shown in the example uses the built-in UART, which is a piece of physical hardware inside the Arduino. Parsing the command line arguments is done in software, within your Arduino program.

## See Also
For more insights, check these out:

1. [Arduino Serial communication](https://www.arduino.cc/en/Serial.Read)
2. [Parsing command line arguments in classic C](https://crasseux.com/books/ctutorial/argc-and-argv.html)
3. [Arduino UART Tutorial](https://www.makerguides.com/arduino-serial-communication/)