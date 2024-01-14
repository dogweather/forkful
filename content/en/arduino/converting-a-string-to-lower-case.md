---
title:                "Arduino recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why

Arduino programming is a useful skill to have, especially for hobbyists and students interested in electronics and robotics. One particular task that may come up is converting a string to lower case. This can be useful for various purposes, such as data manipulation and comparison.

## How To

To convert a string to lower case in Arduino, you can use the `toLowerCase()` function. This function converts all the characters in a string to lower case and returns the updated string. Take a look at the following example:

```Arduino 
String myString = "Hello World";
myString.toLowerCase();
```

The output of this code would be `"hello world"`, with all the characters converted to lower case. You can also assign the converted string to a new variable, as shown below:

```Arduino
String myString = "Hello World";
String lowercaseString = myString.toLowerCase();
```

The output of `lowercaseString` would be the same as before, "hello world". This is a simple yet useful function that can save you time and effort when working with strings in your Arduino projects.

## Deep Dive

If you're curious about how the `toLowerCase()` function works, it uses the ASCII (American Standard Code for Information Interchange) codes to convert the characters to lowercase. Each character has an assigned ASCII code, and converting to lowercase involves adding 32 to the ASCII code of uppercase letters. Other characters, such as numbers and symbols, remain unchanged.

Additionally, you can also use the `toUpperCase()` function to convert a string to uppercase in Arduino using the same concept.

## See Also

- [Arduino Documentation on String Functions](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/)
- [ASCII Table](https://www.asciitable.com/)