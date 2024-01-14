---
title:                "Arduino recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why

In programming, text manipulation is a vital skill to have. One common task is capitalizing a string, which means changing all the letters in a text from lowercase to uppercase. This can be useful in many applications, such as formatting user input or displaying information in a consistent manner. In this blog post, we will discuss how to capitalize a string using Arduino programming language.

## How To

To capitalize a string in Arduino, we will use the `toUpperCase()` function. This function is included in the `string` library, so make sure to include it at the top of your code with `#include <string>`. Let's look at an example:

```Arduino
#include <string> //include string library

String myString = "hello, world!";  //declare a string variable

myString.toUpperCase(); //use toUpperCase() function to capitalize the string

Serial.println(myString); //print the capitalized string to the serial monitor

```

The output of this code will be `HELLO, WORLD!` since `toUpperCase()` converts all letters in a string to uppercase. You can also save the result in a new string variable like this:

```Arduino
#include <string>

String original = "arduino";

String modified = original.toUpperCase(); //save the capitalized string in a new variable

//print both strings to compare
Serial.print(original);
Serial.println(modified);
```

The output of this code will be `arduino` and `ARDUINO` respectively.

## Deep Dive

The `toUpperCase()` function accepts no parameters and simply converts all the letters in a string to uppercase. However, it does not change the original string. If you want to modify the string, you need to save the result in a new variable. This function uses the ASCII table to convert lowercase letters to uppercase. In the ASCII table, uppercase letters range from 65 to 90, while lowercase letters range from 97 to 122. `toUpperCase()` simply subtracts 32 from the ASCII value of each letter, which converts it to its uppercase equivalent.

Keep in mind that this function only works for letters. If there are any non-alphabetic characters in the string, they will not be affected.

## See Also

For more information about text manipulation in Arduino, check out these resources:

- [String library reference](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/touppercase/)
- [Character encoding and ASCII](https://www.arduino.cc/reference/tr/language/structure/character-encoding/)
- [Introduction to strings in Arduino](https://www.arduino.cc/en/Tutorial/StringIntroduction)