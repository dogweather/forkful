---
title:                "Deleting characters matching a pattern"
html_title:           "Arduino recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why?

Deleting characters that match a certain pattern is a common task in programming. This involves removing specific characters from a given input string that meet a certain condition, such as being a vowel or a number. Programmers do this in order to filter out unwanted or irrelevant information and manipulate the data in a more efficient way.

## How to:

To delete characters that match a pattern in Arduino, we can use the ```replace()``` function. This function takes in three parameters: the input string, the character to be replaced, and the replacement character. Here's an example of how we can use it to remove all vowels from a string and print the result:

```Arduino
String input = "Hello world!";
input.replace("a", "");
input.replace("e", "");
input.replace("i", "");
input.replace("o", "");
input.replace("u", "");
Serial.println(input);
```

This will output: "Hll wrld!". As you can see, all the vowels have been removed from the original string.

## Deep Dive:

Historically, deleting characters matching a pattern was a tedious process that involved using loops and conditional statements. This method was not only time-consuming, but also prone to errors. With the introduction of string manipulation functions in modern programming languages like Arduino, the process has become much simpler and more efficient.

An alternative to using the ```replace()``` function is using regular expressions. These expressions allow for more complex patterns to be matched and replaced in a string. However, they may be more difficult for beginners to understand and implement compared to the straightforward ```replace()``` function.

In terms of implementation, the ```replace()``` function uses the ```indexOf()``` function to locate the position of the character to be replaced in the input string, and then replaces it with the replacement character. This process is repeated for each character that matches the pattern.

## See Also:

To learn more about string manipulation in Arduino, check out the official documentation [here](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/). You can also explore different functions for manipulating strings, such as ```substring()``` and ```toUpperCase()```, for more advanced applications.