---
title:                "Arduino recipe: Deleting characters matching a pattern"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why
Are you tired of manually deleting repetitive characters in your Arduino programming? Or, do you want to save time and effort by automatically deleting characters that match a certain pattern? Deleting characters matching a pattern can be a useful tool in simplifying and streamlining your code. 

## How To
To start, we will need to include the string library in our Arduino code. This will allow us to use the `replace()` function which will help us in deleting characters.  

```Arduino
#include <string.h>

void setup() {

}
```

Next, we will declare a string variable and assign it a value. This value can be any string of characters, but for this example, we will use "Hello World!".

```Arduino
void setup() {
  String myString = "Hello World!"; 
}
```
Now, we can use the `replace()` function to delete any character that matches a given pattern. In this case, we want to delete all the uppercase letters in our string. We can use the `replace()` function to replace the uppercase letters with an empty string, effectively deleting them. 

```Arduino
void setup() {
  String myString = "Hello World!";
  myString.replace("H", ""); //replace all uppercase H's with empty string
  myString.replace("W", ""); //replace all uppercase W's with empty string
  Serial.println(myString); //prints "ello orld!"
}
```

We can also use the `replace()` function to delete a specific sequence of characters. For example, if we want to delete the word "World" from our string, we can use the `replace()` function to replace it with an empty string.

```Arduino
void setup() {
  String myString = "Hello World!";
  myString.replace(" World", ""); //replace "World" with empty string
  Serial.println(myString); //prints "Hello!"
}
```

## Deep Dive
The `replace()` function has two parameters, the target character or string and the replacement character or string. It returns a new string with the specified characters replaced. This function can also be used to replace a single character with multiple characters or to replace multiple characters with a single character. 

Additionally, we can use the `replace()` function to delete characters from a specified index onwards. For example, if we want to delete all characters starting from index 5 in our string, we can use the `replace()` function with a third parameter specifying the number of characters to delete.

```Arduino
void setup() {
  String myString = "Hello World!";
  myString.replace(5, 6, ""); //replace all characters from index 5 onwards with empty string
  Serial.println(myString); //prints "Hello!"
}
```

There are also other string manipulation functions that can be combined with the `replace()` function to achieve more complex deletion tasks. These include `substring()` and `indexOf()`. 

## See Also
- [Arduino Reference - String Functions](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/)
- [W3Schools - Arduino Strings](https://www.w3schools.com/arduino/arduino_strings.asp)
- [Tutorialspoint - Arduino String Manipulation](https://www.tutorialspoint.com/arduino/arduino_string_manipulation.htm)