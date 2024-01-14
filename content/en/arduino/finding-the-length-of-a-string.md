---
title:                "Arduino recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why

Are you an Arduino enthusiast looking to expand your programming skills? Have you ever wondered how to find the length of a string in your Arduino code? This blog post will guide you through the process of finding the length of a string in Arduino programming.

## How To

To find the length of a string in Arduino, follow these simple steps:

1. Declare a string variable
   ```Arduino
   String myString = "Hello World!";
   ```
2. Use the `length()` function to find the length of the string
   ```Arduino
   int stringLength = myString.length();
   ```
3. Print the result
   ```Arduino
   Serial.println(stringLength);
   ```
4. Upload the code to your Arduino board and open the Serial Monitor to see the output
   ```Arduino
   Hello World!
   Length of string: 12
   ```

## Deep Dive

Behind the scenes, the `length()` function uses a built-in Arduino function called `strlen()` which stands for "string length". This function counts the number of characters in a string, starting from the first character and continuing until it reaches the null terminator, which indicates the end of the string. It is important to note that the `length()` function does not include the null terminator in the count, which is why the length of "Hello World!" is 12 instead of 13.

You may also come across another function called `size()`, which also returns the length of a string. However, `size()` is not specifically designed for strings and is typically used for arrays. It is recommended to use `length()` for finding the length of a string in Arduino.

## See Also

For more information on string functions in Arduino, you can check out the official Arduino documentation or these helpful resources:

- [Arduino String Length: How to Find Length of String in Arduino](https://www.tutorialspoint.com/arduino/arduino_string_length.htm)
- [Using Strings in Arduino Sketches](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/)
- [Arduino String Functions with Practical Examples](https://electronicsforu.com/electronics-projects/arduino-string-functions-with-examples)

Now that you know how to find the length of a string in Arduino, you can use this knowledge to create more complex and dynamic projects. Happy coding!