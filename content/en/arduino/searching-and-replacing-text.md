---
title:                "Searching and replacing text"
html_title:           "Arduino recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why
Let's face it, nobody enjoys manually going through lines of code to replace a specific string. It's time-consuming and prone to human error. Save yourself the headache and let your Arduino do the work for you!

## How To

First, let's define the variables we'll be using:
```Arduino
String originalString = "Hello, world!";
String newString = "Hi, there!";
String currentLine;
```

Next, we'll use the `replace()` function to search for and replace the original string with the new one:
```Arduino
currentLine = originalString.replace("Hello", "Hi");
```

And that's it! The variable `currentLine` now stores the new string "Hi, world!". You can also use this function to replace multiple instances of a string within a larger text.

## Deep Dive
The `replace()` function is part of the String library in Arduino and has the following syntax:
```Arduino
string.replace(oldValue, newValue)
```
This function can also be used with variables as the oldValue and newValue parameters, making it more versatile for different situations. Additionally, the `replace()` function only modifies the specified string and does not affect the original one.

Some other useful String functions for searching and replacing include `indexOf()` and `lastIndexOf()`, which return the index of the first/last occurrence of a specified string, and `substring()`, which allows you to extract a portion of a string.

## See Also
Still curious about working with strings in Arduino? Check out these helpful links:
- Arduino Reference - [String](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- Arduino String Examples - [Manipulation](https://www.arduino.cc/en/Tutorial/BuiltInExamples/StringManipulation)
- Arduino Forum - [String Manipulation in Arduino](https://forum.arduino.cc/t/string-manipulation-in-arduino/88197)