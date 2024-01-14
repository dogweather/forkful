---
title:                "Arduino recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why
Substrings are a powerful feature in programming that allow us to extract smaller portions of a string, or a sequence of characters, from a larger string. This can be useful for tasks such as parsing data, manipulating text, or performing calculations. In Arduino programming, substrings can be particularly useful for working with sensor data or user input.

## How To
To extract substrings in Arduino, we can use the `substring()` function. Let's look at an example where we have a string variable `message` with the value "Hello World!". We want to extract the substring "World" from this string and print it out.

```
Arduino String message = "Hello World!"; 
String substring = message.substring(6, 11); 
Serial.print(substring);
```

In this code, we first declare a String variable `message` with the value "Hello World!". Then, we create another String variable `substring` and use the `message.substring()` function to extract the characters from position 6 to 11. Note that the position numbering starts at 0, so the "W" in "World" is at position 6 and the "d" is at position 10. Finally, we print out the value of `substring` using the `Serial.print()` function.

When we run this code, the output will be "World" in the serial monitor. We have successfully extracted a substring from our original string!

## Deep Dive
The `substring()` function takes two parameters: the starting position and the ending position of the substring. We can also include a third parameter for the length of the substring, but if we only specify the starting and ending positions, the function will return all characters between those two positions, including the character at the starting position but not including the character at the ending position.

Additionally, the `substring()` function can be used to extract a substring from an array of characters instead of a String variable. We simply need to pass the array name and the number of characters we want to extract as parameters, like this:

```
char message[] = "Hello World!"; 
String substring = message.substring(6, 11); 
```

Now, the `substring()` function will return the characters "World" from the array `message[]`.

## See Also
To learn more about substrings in Arduino programming, check out these resources:

- [Arduino Reference for `substring()` function](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/)
- [Tutorial on extracting substrings in Arduino](https://create.arduino.cc/projecthub/imad96/extracting-substrings-in-arduino-501140)
- [Arduino String Manipulation Tutorial](https://www.youtube.com/watch?v=UEy-i7ZpLrA&t=3s)