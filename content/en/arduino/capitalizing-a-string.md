---
title:                "Capitalizing a string"
html_title:           "Arduino recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Capitalizing a string is the process of converting all the letters in the text to uppercase. Programmers often use this to standardize data inputs, improving readability and facilitating data analysis.

## How to:
You can capitalize a string in Arduino using the `toUpperCase()` function linked to a string object. 

Here's a quick example:

```Arduino
String sentence = "this is an Example sentence.";
sentence.toUpperCase();
Serial.begin(9600);
Serial.println(sentence);
```

After uploading this code to your Arduino board and opening your serial monitor, you will see this:

```Arduino
THIS IS AN EXAMPLE SENTENCE.
```

All the characters in the string are now in uppercase. 

## Deep Dive
Although the `toUpperCase()` function is convenient, it hasn't always been available. In previous versions of Arduino language, people would create a custom function to change lowercase characters to uppercase. This would involve ASCII value conversion, and it's not as efficient.

An alternative way to convert a string to uppercase is using character arrays in combination with the `toupper()` function. However, this method is recommended for more experienced coders as it requires a good understanding of pointers and memory management in C++:

```Arduino
char str[] = "example sentence";
for(int i=0; str[i]!='\0'; i++) str[i]=toupper(str[i]);
Serial.begin(9600);
Serial.println(str);
```

The `toUpperCase()` function operates by iterating through the string and converting each character separately. Essentially, it automates the `for` loop used in the alternative method. It's worth noting that `toUpperCase()` only operates on ASCII characters, special or Unicode characters may not be affected.

## See Also
For more insights and examples in Arduino Programming, visit:

1. [Official Arduino Language Reference](https://www.arduino.cc/reference/en/)
2. [Arduino String Object - Programming Electronics Academy](https://www.programmingelectronics.com/tutorial-18-break-arduino-guide-understanding-programming-syntax/)
3. [Arduino Text Strings - Adafruit](https://learn.adafruit.com/adafruit-arduino-lesson-22-arduino-%20-%20%20%20-%20-%20%20-%20%20-%20%20%20-%20-%20-%20%20-%20%20%20-%20%20%20-%20-%20-%20%20-%20%20%20-%20%20%20-%20-%20-%20%20-%20%20%20-%20lcd-display-part-1/arduino-%20-%20%20%20-%20-%20%20-%20%20-%20%20%20-%20-%20-%20%20-%20%20%20-%20%20%20-%20-%20-%20%20-%20%20%20-%20%20%20-%20-%20-%20%20-%20%20%20-%20text-strings)