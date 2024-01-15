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

## Why
There are many reasons why you might want to delete characters from a string that match a specific pattern. It could be to filter out unwanted information, remove formatting, or simply to make the string more readable.

## How To
To delete characters matching a pattern in Arduino, you can use the `replace()` function from the `String` library. This function takes two parameters: the pattern to look for, and the replacement string. For example, if we have a string called `myString` and we want to remove all spaces from it, we could do the following:

```Arduino
myString.replace(" ", "");
```

This will replace all spaces in `myString` with an empty string, effectively deleting them. You can also use wildcards in the pattern, such as `*` to delete all characters between two specific characters. Let's say we have a string `myName` and we want to remove all characters between the letters "a" and "n":

```Arduino
myName.replace("a*n", "");
```

This will delete all characters between the letters "a" and "n" (including both letters).

## Deep Dive
The `replace` function uses regular expressions to match and replace patterns in a string. Regular expressions are a special syntax for specifying patterns in text. If you want to learn more about regular expressions, you can check out this [Arduino tutorial](https://www.arduino.cc/reference/en/language/functions/string/functions/replace/) or this [documentation from the PCRE library](https://pcre.org/current/doc/html/pcre2syntax.html). 

Additionally, the `replace` function is just one of many string manipulation functions available in the `String` library. You can also use functions like `remove`, `substring`, and `startsWith` to customize the manipulation of your strings. For a full list of string functions and their usage, check out the [Arduino String Reference](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/).

## See Also
- [Arduino String Reference](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [Arduino String library documentation](https://github.com/arduino-libraries/String)
- [Regular Expression Tutorial](https://medium.com/factory-mind/regex-tutorial-a-simple-cheatsheet-by-examples-649dc1c3f285)