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

## Why

Are you tired of manually entering input values every time you run your Arduino program? Are you looking for a more efficient and dynamic way to interact with your projects? By learning how to read command line arguments, you can easily pass in parameters to your program without the need for constant input.

## How To

Reading command line arguments in Arduino involves using the ```Serial``` library and the ```argc``` and ```argv``` variables. Let's take a look at a simple program that reads in two numbers from the command line and prints their sum:

```Arduino
#include <stdio.h>
int sum = 0;

void setup() {
 Serial.begin(9600);
}

void loop() {
 if (Serial.available() > 0) {
  int num1, num2;
  // read in first number
  num1 = atoi(Serial.readStringUntil(' ').c_str());
  // read in second number
  num2 = atoi(Serial.readStringUntil('\n').c_str());
  // calculate sum
  sum = num1 + num2;
  // print result
  Serial.print("The sum of ");
  Serial.print(num1);
  Serial.print(" and ");
  Serial.print(num2);
  Serial.print(" is: ");
  Serial.println(sum);
 }
}

```

Input: 5 7
Output: The sum of 5 and 7 is: 12

In this program, we use the ```Serial``` library to read in the input from the serial monitor. The ```Serial.available()``` function checks if there is any data available to be read. We then use the ```Serial.readStringUntil()``` function to read in the numbers until a space or a newline character is encountered. These numbers are converted to integers using the ```atoi()``` function and then added together to get the sum. Finally, we use the ```Serial.print()``` and ```Serial.println()``` functions to print out the result on the serial monitor.

## Deep Dive

Using ```argc``` and ```argv```, we can also read in command line arguments directly from the Arduino IDE. If we modify our previous program to include these variables, we can pass in our input as command line arguments when running the program. Let's take a look at an example:

```Arduino
#include <stdio.h>
int sum = 0;

int main(int argc, char **argv) {
 int num1, num2;
 // read in first number
 num1 = atoi(argv[1]);
 // read in second number
 num2 = atoi(argv[2]);
 // calculate sum
 sum = num1 + num2;
 // print result
 Serial.print("The sum of ");
 Serial.print(num1);
 Serial.print(" and ");
 Serial.print(num2);
 Serial.print(" is: ");
 Serial.println(sum);
}
```

Input: 5 7
Output: The sum of 5 and 7 is: 12

In this program, we use ```argc``` to determine the number of command line arguments passed in and ```argv``` to access each argument. The first argument (```argv[0]```) is the name of the program, so we start reading our input from the second argument (```argv[1]``` and ```argv[2]```). This allows for a more automated way to pass in input without having to manually enter it in the serial monitor.

## See Also

- [Arduino Serial](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [Serial.available()](https://www.arduino.cc/reference/en/language/functions/communication/serial/available/)
- [Serial.readStringUntil()](https://www.arduino.cc/reference/en/language/functions/communication/serial/readstringuntil/)