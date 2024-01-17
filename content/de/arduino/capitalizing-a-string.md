---
title:                "Großschreibung eines Strings"
html_title:           "Arduino: Großschreibung eines Strings"
simple_title:         "Großschreibung eines Strings"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
 
Capitalizing a string is the act of changing the first letter of each word in a sentence or phrase to its uppercase form. Programmers often do this to make their code more readable and organized. It also helps to identify important words or variable names within the code. 

## Wie man es macht:

For this task, we will use the built-in ```toUpperCase()``` function in Arduino. This function takes in a string as an argument and returns the same string with all letters in uppercase. Let's see an example:

```
// Define a string variable
String name = "arduino programming";

// Capitalize the string
name = name.toUpperCase();

// Print the result
Serial.println(name);

// Output: ARDUINO PROGRAMMING
```

## Tief tauchen:

This method of capitalizing strings has been used for a long time in programming languages. However, there are also alternative methods such as creating a custom function to capitalize strings. Additionally, it is important to note that this method only capitalizes letters based on their ASCII value, and may not work for all languages. For a more in-depth look, you can refer to the [Arduino documentation](https://www.arduino.cc/en/Reference/StringToUpper). 

## Siehe auch:

- [String data type in Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [ASCII table for character values](https://www.ascii-code.com/)