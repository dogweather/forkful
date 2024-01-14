---
title:                "Arduino recipe: Capitalizing a string"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why

Capitalizing a string may seem like a minor task, but it can have significant impacts on the overall functionality and usability of your Arduino project. By capitalizing a string, you can ensure that user inputs or data are consistently formatted and easier to read and process.

## How To

To capitalize a string in Arduino, we can use the `String.toUpperCase()` function. This function takes in a string variable and converts all lowercase letters to uppercase letters. Let's see an example of how this works:

```
Arduino String myString = "hello world";
myString.toUpperCase();
Serial.println(myString);
```

This code will output the following: `HELLO WORLD`. As you can see, the `toUpperCase()` function has converted all the lowercase letters in the string to uppercase.

You can also use this function in combination with other string manipulation functions such as `charAt()` and `substring()` to selectively capitalize certain parts of a string. For example:

```
Arduino String myString = "my favorite color is blue";
myString.toUpperCase();
myString.setCharAt(20, 'B');
Serial.println(myString);
```

This code will output: `MY FAVORITE COLOR IS Blue`.

## Deep Dive

Behind the scenes, the `toUpperCase()` function uses the ASCII values of each character to convert lowercase letters to uppercase. The ASCII value of a lowercase letter is 32 less than the uppercase equivalent. So, the function simply subtracts 32 from the ASCII value of each lowercase character to convert it to uppercase. This is why capitalizing a string may seem like a minor task, as it requires basic mathematical operations.

It's worth noting that the `toUpperCase()` function does not change the original string, but instead returns a new string with the converted characters. This is why we need to assign the function's output to a new string variable.

## See Also

- [Arduino String Class Reference](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/touppercase/)
- [ASCII Character Codes](https://www.asciitable.com/)
- [String Manipulation in Arduino](https://create.arduino.cc/projecthub/Chamuth/String-Manipulation-in-Arduino-7bb0c9)