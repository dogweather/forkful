---
title:                "Using regular expressions"
html_title:           "Arduino recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?

Regular expressions, also known as "regex", are a powerful tool used by programmers to search, manipulate, and validate text patterns within a given string of characters. It allows for more efficient and accurate text processing, making it an essential skill for any programmer.

## How to:

To use regular expressions in Arduino, you will first need to include the "Regex" library. Here's an example of searching for a specific pattern in a string:

```Arduino
#include <Regex.h>  //include the library

void setup() {

  String text = "Hello there!";  //our string
  regex pattern = regex("Hello");  //our pattern to search for
  bool match = Regex.match(pattern, text);  //check for a match

  if (match) {  //if a match is found
    Serial.println("Pattern found!");  //print confirmation
  }
}

void loop() {
  //nothing to do here
}
```

Output:
```
Pattern found!
```

Similarly, we can use regular expressions to replace a specific pattern with a new string. Here's an example:

```Arduino
#include <Regex.h>  //include the library

void setup() {

  String text = "I love ice cream.";  //our string
  regex pattern = regex("ice cream");  //our pattern to replace
  String replacement = "pizza";  //our replacement string

  Regex.replace(pattern, text, replacement);  //replace the pattern with the new string
  Serial.println(text);  //print the updated string
}

void loop() {
  //nothing to do here
}
```

Output:
```
I love pizza.
```

## Deep Dive:

Regular expressions have been around since the 1950s and have become a standard feature in many programming languages. However, they can be quite complicated to learn and can seem daunting to beginners. Fortunately, there are many online resources and tutorials available that can help you learn and understand regular expressions better.

Alternatively, there are also other methods of achieving similar results without using regular expressions, such as string manipulation functions and bitwise operations. However, regular expressions offer a more concise approach and are often faster and more efficient.

When using regular expressions in Arduino, it's essential to understand that they consume valuable memory and processing power. Therefore, they should only be used when necessary and with caution in memory-constrained projects.

## See Also:

To learn more about regular expressions and how to use them in Arduino, check out these resources:
- [Arduino Regex Library Documentation](https://www.arduino.cc/reference/en/libraries/regex/)
- [Regular Expressions Tutorial by W3Schools](https://www.w3schools.com/regex/)
- [Regular Expressions Cheat Sheet](https://www.cheatography.com/davechild/cheat-sheets/regular-expressions/)