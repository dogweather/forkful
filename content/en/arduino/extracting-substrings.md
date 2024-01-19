---
title:                "Extracting substrings"
html_title:           "Arduino recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?
Substring extraction in Arduino involves isolating a specific portion, or "substring", from a longer string of text. This maneuver allows programmers to extract, analyze or manipulate data more precisely and efficiently.

## How to:
Let's dive in and see a simple example using the `substring()` function.

```Arduino
String str = "Hello, Arduino!";
String newStr = str.substring(7, 15);
Serial.begin(9600);
Serial.println(newStr); // Prints "Arduino".
```
In the above code, `str.substring(7, 15)` extracts the substring starting at index 7 and ending at index 14 (note that 15th index is exclusive), which would print "Arduino". If you just input one parameter, it will start at that index and continue to the end of the string.

```Arduino
newStr = str.substring(7);
Serial.println(newStr); // prints "Arduino!".
```

## Deep Dive:
The `substring()` feature in Arduino's string class has its roots set deeply in many programming languages, whether it's Python's slicing technique or Java's equivalent `substring()` function. 

Alternatives? Sure. For instance, you could manually iterate through the characters. But this is inefficient and arduous. Arduino's `substring()` offers a clean, ready-to-use solution.

The `substring()` implementation is pretty straightforward. It instantiates a new String (making it a potentially memory-intensive operation), and copies the required characters from the original string to it.

## See Also:
To further increase your Arduino knowledge, consider checking out:
- Arduino's official String page: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- Dig into more complex string manipulation techniques: https://www.arduino.cc/en/Tutorial/LibraryExamples/TextString
- Understanding memory management in Arduino strings: https://learn.adafruit.com/memories-of-an-arduino/optimizing-sram