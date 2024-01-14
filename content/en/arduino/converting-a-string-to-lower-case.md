---
title:                "Arduino recipe: Converting a string to lower case"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why
Converting strings to lower case is a common task in programming, and it is essential to ensure consistency in data processing and comparison. Whether you are creating a chatbot or a data analysis tool, being able to convert strings to lower case can help in handling user input and handling data more efficiently.

## How To
To convert a string to lower case in Arduino, you can use the `toLowerCase()` function from the `String` library. This function takes a string as input and returns a new string with all characters converted to lower case.

Here is an example code snippet for converting a string to lower case in Arduino:

```
ArduinoString str = "CONVERT TO LOWER CASE";
ArduinoString lowercaseStr = str.toLowerCase();
Serial.println(lowercaseStr);
```

The expected output of this code would be:

```
convert to lower case
```

## Deep Dive
In Arduino, strings are objects of the `String` class and can be manipulated using various inbuilt functions. The `toLowerCase()` function is an example of such a function that helps in converting strings to lower case.

Under the hood, the `toLowerCase()` function loops through each character of the string, checks its ASCII value, and converts it to its corresponding lower case character. This process continues until all characters in the string have been converted.

Keep in mind that the `toLowerCase()` function creates a new string and does not modify the original string. So, if you need to use the converted string later in your code, make sure to store it in a new variable.

## See Also
- [Arduino documentation for String.toLowerCase()](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/tolowercase/)
- [Convert String to Lower Case in Arduino](https://howtomechatronics.com/tutorials/arduino/convert-string-to-lower-case-tutorial/)

By using the `toLowerCase()` function, you can easily convert strings to lower case in your Arduino projects. This simple yet powerful function can save you time and effort in handling string data. Happy coding!