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

## What & Why?

Searching and replacing text is changing specific string values within a larger body of text. Programmers do it to either edit specific string values or to manipulate data by altering certain parts of it.

## How to:

In Arduino, a common way to do this is using the `replace()` function from the String class. Here's a quick example:

```Arduino
String text = "Hello, Arduino!";
text.replace("Arduino", "World");
Serial.println(text);  // Output: "Hello, World!"
```
In this example, the word "Arduino" in the string "text" will be replaced with "World." The modified string is then printed to the serial monitor.

## Deep Dive

Searching and replacing text isn't new. It's been an integral part of programming and text editing since the days of early text editors. The Arduino `replace()` function is one aspect of this.

An alternative to using `replace()` is to manually find the index of the substring you want to change using `indexOf()`, and then manually replacing it using `substring()`. However, this is more complex and not recommended for simple replacements.

One thing to note about how `replace()` works is that it replaces the _first_ occurrence of the specified substring in the string. If you want to replace _all_ occurrences, you'd typically use a loop.

## See Also:

For further reading on text manipulation techniques in Arduino, see the official Arduino String reference at: https://www.arduino.cc/reference/en/language/variables/data-types/string/
As well as this great article on string manipulation techniques from Bald Engineer: https://www.baldengineer.com/arduino-string-manipulation-using-minimal-ram.html