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
Capitalizing a string means converting all of its letters to uppercase. Programmers often do this to standardize data or to make it easier to search for specific words or phrases within a string.

## How to:
To capitalize a string in Arduino, use the `toUpperCase()` function. This function takes in a string as its argument and returns the capitalized version of the string.

```
Arduino String myString = "hello world";
myString.toUpperCase(); // will return "HELLO WORLD"
```

To save the capitalized string, assign the returned value to a new string variable.

```
Arduino String myString = "hello world";
Arduino String capitalizedString = myString.toUpperCase(); // will return "HELLO WORLD"
```

If you want to print the capitalized string to the serial monitor, use the `println()` function.

```
Arduino String myString = "hello world";
Serial.println(myString.toUpperCase()); // will print "HELLO WORLD" to the serial monitor
```

## Deep Dive:
Capitalizing strings has been a common practice in computer programming for decades. It can be useful in situations where data needs to be uniform or when performing string searches. 

An alternative to the `toUpperCase()` function is the `toUpper()` function, which is part of the `<ctype.h>` library. This function takes in a character as its argument and returns the uppercase version of that character. This means that to capitalize a string using `toUpper()`, you would need to iterate through each character in the string and convert them one by one. However, the `toUpperCase()` function already does this for you, making it a more efficient option.

In terms of implementation, the `toUpperCase()` function is part of the Arduino `String` class, which is specifically made for manipulating strings. This class also has other useful functions for string manipulation, such as `concat()` and `substring()`. These functions can come in handy when working with data that requires string processing.

## See Also:
- Arduino String Reference: https://www.arduino.cc/reference/en/language/variables/data-types/string/
- C Programming - String Functions: https://www.tutorialspoint.com/c_standard_library/string_h.htm