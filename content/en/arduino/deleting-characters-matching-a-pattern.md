---
title:                "Deleting characters matching a pattern"
html_title:           "Lua recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why?

Occasionally, programmers find a need to delete characters in a string that follow a specific pattern - this is known as pattern matching. This neat trick cleans up your data and simplifies further data analysis!

## How to:

Deleting characters matching a pattern in Arduino is straightforward. Here is an example. The function `deleteChar()` is what we're after.

```Arduino
void setup()   {
  Serial.begin(9600);
}

void loop()
{
  String text = "Hello#World";
  deleteChar(text, '#');
  Serial.println(text); //Prints "HelloWorld"
  delay (1000);
}

void deleteChar(String &s, char c) {
  s.remove(s.indexOf(c));
}
```

In the snippet above, the character '#' is deleted from the string "Hello#World", outputting "HelloWorld".

## Deep Dive

Though there's no native function in C++ to delete a char, Arduino's String Class fills the gap with the `remove()` function. Before Arduino was around, the same could've been achieved with a cumbersome iteration through a string. Nowadays, this clean, alternative way of deleting characters from strings hardly takes any time!

When choosing an alternative to the `remove()` function, one must understand the performance trade-off. The standard `remove()` function performs adequately for small text sizes. However, when dealing with larger strings or frequent deletions, it might make sense to use a more efficient string manipulation library.

Lastly, let's draw focus to the implementation aspect. Remember to pass the string by reference (`String &s`) - this ensures your function manipulates the original string and not a copy!

## See Also

For more on Arduino's String class: [Arduino String Functions](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/)

Dig deeper into String Libraries in C++: [C++ String Library - cplusplus.com](http://www.cplusplus.com/reference/string/string/)