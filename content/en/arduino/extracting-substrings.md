---
title:                "Extracting substrings"
html_title:           "Arduino recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?

Extracting substrings refers to the process of extracting a smaller portion of characters from a larger string. This is a commonly used technique in programming to manipulate and access specific parts of a string. In simpler terms, it allows you to easily grab the parts of a string that you need, without having to work with the entire string every time.

## How to:
Extracting substrings is made easy with the use of built-in functions in the Arduino programming language. Here are some examples of how to extract substrings:

```Arduino
// Extracting a single character at a specific index using charAt()
String str = "Hello World";
char character = str.charAt(4); //character = 'o'
```

```Arduino
// Extracting a substring with a specific starting index and length using substring()
String str = "Hello World";
String subStr = str.substring(3, 7); //subStr = "lo W"
```

```Arduino
// Extracting a substring from a specific index until the end of the string using substring()
String str = "Hello World";
String subStr = str.substring(6); //subStr = "World"
```

## Deep Dive:
The ability to extract substrings has been a staple in programming languages for decades, and it continues to be a useful tool for developers. Before the introduction of built-in functions, programmers had to manually manipulate strings using techniques like pointers and array slicing. This was time-consuming and error-prone, making the addition of substring extraction functions a welcome feature.

While there are alternative methods for extracting substrings, such as regular expressions, they are often more complex and may not be supported in all programming languages. Therefore, it is important for programmers to be familiar with the built-in substring functions in their programming language of choice.

When implementing substring extraction, it is important to consider the starting index and length of the desired substring. Different programming languages may have different conventions for specifying these values. In Arduino, the starting index is designated as the "beginIndex" and the length is designated as the "length" in the substring() function.

## See Also:
To learn more about substring extraction in Arduino and how to use it in your projects, check out these resources:
- [Arduino Reference - String Functions](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/)
- [Arduino Tutorial - Manipulating Strings](https://www.arduino.cc/en/Tutorial/StringManipulation)