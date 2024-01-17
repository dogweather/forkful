---
title:                "Finding the length of a string"
html_title:           "Arduino recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Finding the length of a string is the process of determining the number of characters in a string variable. This is a common task for programmers, especially when dealing with user input or manipulating strings in their code.

## How to:

```Arduino
//First, declare a string variable and assign a value
String myString = "Hello World!";

//Use the length() function to find the length of the string
int length = myString.length();

//Print the length to the serial monitor
Serial.println(length);

//Output: 12
```

## Deep Dive:

### Historical Context:

In early programming languages, strings were represented as arrays of characters and the length was determined by counting the elements in the array. However, with the introduction of string data types, the length() function became a built-in method for finding the length of a string.

### Alternatives:

Another way to find the length of a string is by using the strlen() function in C++. However, this function only works with null-terminated strings and may not be compatible with all string variables.

### Implementation Details:

The length() function in Arduino returns the number of characters in a string, including any spaces or special characters. If you want to exclude spaces or certain characters, you can use the trim() function to remove them before finding the length.

## See Also:

- [Arduino String Reference](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [C++ strlen() function](https://www.geeksforgeeks.org/strlen-function-in-cpp/)
- [Arduino trim() function](https://www.arduino.cc/reference/en/language/variables/data-types/string-functions/trim/)