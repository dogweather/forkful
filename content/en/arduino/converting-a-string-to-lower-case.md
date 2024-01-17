---
title:                "Converting a string to lower case"
html_title:           "Arduino recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?

Converting a string to lower case is the process of changing all uppercase letters in a string to their corresponding lowercase letters. Programmers often do this to ensure consistency and uniformity in user input. It also makes searching and comparing strings easier, as uppercase and lowercase letters are treated differently in programming.

## How to:

To convert a string to lower case in Arduino, we can use the ```toLowerCase()``` function. This function takes in a string as an argument and returns a new string with all lowercase letters. Let's see an example:

```Arduino
String str = "Hello World";
String lowerStr = str.toLowerCase();

Serial.println(lowerStr); // Output: hello world
```

In this example, we create a string variable with the value "Hello World." Then, using the ```toLowerCase()``` function, we convert the string to lowercase and store it in a new variable called ```lowerStr```. Lastly, we use the ```Serial.println()``` function to print the lowercase string to the serial monitor.

## Deep Dive:

Converting strings to lowercase has been a common practice in programming languages since the early days of computing. It allows for more flexibility in user input and text manipulation. In some programming languages, there are also other methods for converting strings to lowercase, such as using loops or regular expressions.

An alternative to converting strings to lowercase is to use the ```toUpperCase()``` function to convert all letters to uppercase. However, this may not always be ideal depending on the specific use case.

In terms of implementation, converting a string to lowercase involves iterating through each character in the string and checking if it is a lowercase letter. If it is, we leave it as is. Otherwise, we convert it to lowercase by adding the ASCII value difference between uppercase and lowercase letters. This process is repeated for each character in the string to create the new lowercase string.

## See Also:

- [Arduino Reference - toLowerCase()](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/tolowercase/)
- [ASCII Table](http://www.asciitable.com)