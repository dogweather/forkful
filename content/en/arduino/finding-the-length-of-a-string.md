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

Finding the string's length is figuring out the number of characters within a string. Programmers do it to manipulate, validate, or generally work with string data efficiently.

## How to:

In Arduino, counting the number of text characters inside a string can be done using the `length()` function:

```Arduino
String myString = "Hello World!";
int len = myString.length();
Serial.println(len); //Outputs: 12
```

Running this will print "12" on the serial monitor - representing the length of the string "Hello World!", including the exclamation mark and spaces. 

## Deep Dive:

Historically, before `length()`, programmers counted string characters using custom loops, which was cumbersome. 

One alternative to using `length()` is manually iterating over the string and counting each character:

```Arduino a
String myString = "Hello World!";
int len = 0;
for(int i = 0; i < 1000; i++){
  if (myString.charAt(i) != '\0'){
    len++;
  } else {
    break;
  }
}
Serial.println(len); //Outputs: 12
```

Here is how `length()` works: a string in Arduino is an object, which stores its length inside a property, updated every time that string changes. Calling `length()` just returns this value, hence it's several times quicker than iterating over the string.

## See Also:

1. Official `String.length()` documentation at the Arduino website ([link](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/length/))
2. More about Arduino strings in general ([link](https://learn.adafruit.com/multi-tasking-the-arduino-part-3/overview))
3. String length calculation in traditional C, a different context but good for understanding fundamentals ([link](https://www.tutorialspoint.com/c_standard_library/c_function_strlen.htm))