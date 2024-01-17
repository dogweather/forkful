---
title:                "Reading command line arguments"
html_title:           "Arduino recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?

Reading command line arguments is the process of taking inputs from the command line and using them in a program. Programmers do this to make their programs more versatile by allowing users to customize their experience and provide dynamic inputs.

## How to:

To read command line arguments in Arduino, we can use the `Serial.readString()` function. First, we declare a string variable to store the input data. Then, we use the `Serial.begin()` function to initialize serial communication with the computer. Next, we use a `while` loop to continuously read inputs from the serial port using `Serial.readString()`. Finally, we can use `Serial.println()` to print out the inputs. Here's an example: 

```
Arduino String inputString;
Serial.begin(9600);

while(Serial.available()>0){
  inputString = Serial.readString();
  Serial.println(inputString);
}
```

If the user inputs "Hello World" in the serial monitor, the output would be: "Hello World".

## Deep Dive:

Historically, command line arguments were used in computers without graphical user interfaces as a way to input data and control programs. Nowadays, they are still widely used in programming languages like C and C++ for creating command-line programs. An alternative to reading command line arguments in Arduino is using the `Serial.parseInt()` function, which only reads integer values from the serial port. 

When using the `Serial.readString()` function, it's important to note that the input data is stored in memory and can use up a lot of space if the inputs are too large. Therefore, it's recommended to use `Serial.readStringUntil()` instead, which reads the input until a specific character is encountered. This can save memory usage and improve performance.

## See Also:

For more information on using `Serial.readString()` and `Serial.readStringUntil()`, refer to the official Arduino documentation: https://www.arduino.cc/reference/en/language/functions/communication/serial/readstring/.

To learn more about command line arguments in general, check out this article by FreeCodeCamp: https://www.freecodecamp.org/news/what-are-command-line-arguments-and-why-we-use-them/.

Now you know how to read command line arguments in Arduino, so go ahead and try it out in your own projects! Happy coding!