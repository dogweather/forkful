---
title:                "Converting a string to lower case"
html_title:           "Arduino recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why 
Converting a string to lower case can be useful in various scenarios. For example, if you are reading input from a user and want to standardize the data, converting all letters to lower case can help with data manipulation and comparison. 

## How To
To convert a string to lower case in Arduino, we can use the `toLowerCase()` function. Here's an example code snippet: 

```Arduino
String input = "Hello World";
input.toLowerCase(); //this updates the string variable to "hello world"
Serial.println(input); //prints "hello world" to the serial monitor
```

We can also use a loop to iterate through each character in the string and convert them individually using the `toLowerCase()` function. Here's an example: 

```Arduino
String input = "I Love Arduino";
String output = ""; //declaring an empty string to store the converted string
for(int i=0; i < input.length(); i++){
    output += String(input.charAt(i)).toLowerCase();
}
Serial.println(output); //prints "i love arduino" to the serial monitor
```

## Deep Dive
The `toLowerCase()` function uses the ASCII characters to determine whether a letter needs to be converted to lower case or not. For example, if the ASCII value of a character falls between 65 (A) and 90 (Z), it will be converted to its lower case equivalent by adding 32 to its ASCII value. Similarly, if the ASCII value falls between 97 (a) and 122 (z), no conversion is necessary. 

Apart from the `toLowerCase()` function, there are other methods and libraries available for string manipulation in Arduino. It is recommended to explore these options and choose the most efficient one according to your project's needs. 

## See Also
- [Official Arduino Documentation on Strings](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [Arduino String Library](https://www.arduino.cc/reference/en/libraries/string/) for more advanced string manipulation functions.