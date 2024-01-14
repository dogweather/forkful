---
title:                "Arduino recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why
Have you ever wanted to combine different pieces of text or numbers together to create a longer, more complex statement in your Arduino program? Then you need to learn about string concatenation!

## How To
To concatenate strings in Arduino, we use the `+` operator. Let's take a look at an example:

```Arduino
void setup() {
  Serial.begin(9600);  // Initialize serial communication at 9600 baud
  String name = "Arduino";  // Declare a string variable with the name "Arduino"
  String greeting = "Hello " + name;  // Concatenate the string "Hello" with the variable name
  Serial.println(greeting);  // Prints "Hello Arduino" to the serial monitor
}

void loop() {
  // The code in the loop will repeat continuously
}
```

In this example, we first declare a string variable `name` with the value "Arduino". Then, we create a new string variable `greeting` by concatenating the string "Hello" with the value of `name`. Finally, we print the value of `greeting` to the serial monitor, which displays "Hello Arduino".

String concatenation can also be used to combine multiple variables or constants. Take a look at the code below:

```Arduino
int number = 10;  // Declare an integer variable with the value 10
String message = "The number is: " + String(number);  // Convert the integer to a string and concatenate it with the message
Serial.println(message);  // Prints "The number is: 10" to the serial monitor
```

Here, we have used the `String()` function to convert the integer variable `number` into a string, so it can be concatenated with the rest of the message.

## Deep Dive
String concatenation in Arduino is done using the `String` class. This class has useful methods such as `concat()`, which allows us to concatenate multiple strings together, and `indexOf()`, which can be used to find the position of a specific character or substring within a string.

It is important to note that using string concatenation with large amounts of text or multiple variables can quickly use up the limited memory of the Arduino. In these cases, it is better to use a `char` array instead of a `String` variable.

## See Also
- [Arduino String Class](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/)
- [Arduino String Concatenation Tutorial](https://programmingelectronics.com/arduino-string-concatenation-tutorial/)