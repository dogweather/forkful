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

## Why

Have you ever come across a situation where you need to search for a specific pattern within a string or a large chunk of data? Using regular expressions in your Arduino programming can help you accomplish this task efficiently and accurately.

## How To

Using regular expressions in Arduino is as simple as writing a few lines of code. First, you need to include the regex library by adding the following line at the beginning of your code:

```Arduino
#include <regex.h>
```

Next, you need to create a regular expression pattern using the `regex_t` data type and `regcomp()` function. For example, if you want to search for the word "Hello" in a string, your code would look like this:

```Arduino
char *pattern = "Hello";
regex_t regex;
int result = regcomp(&regex, pattern, 0);
```

After creating the pattern, you can use the `regexec()` function to match the pattern with a string. The `regexec()` function returns zero if the pattern is found in the string, and a non-zero value otherwise. Here's a code snippet to demonstrate this:

```Arduino
char *text = "Hello there!";
int match_found = regexec(&regex, text, 0, NULL, 0);
if (match_found == 0) {
    Serial.println("Pattern found!");
} else {
    Serial.println("Pattern not found!");
}
```

The above code will output "Pattern found!" since the word "Hello" is present in the string.

## Deep Dive

Regular expressions use a set of special characters to represent patterns in a string. Some commonly used characters include:

- `.` : Matches any single character
- `*` : Matches zero or more occurrences of the previous character
- `+` : Matches one or more occurrences of the previous character
- `?` : Matches zero or one occurrence of the previous character
- `[ ]` : Matches any single character in the specified set
- `^` : Matches the beginning of a string
- `$` : Matches the end of a string

Using these characters, you can create complex patterns and search for specific strings within your data.

## See Also

- [Regexp Reference](https://www.regular-expressions.info/reference.html)
- [Arduino Reference](https://www.arduino.cc/reference/en/)
- [Regular Expressions Cheat Sheet](https://cheatography.com/davechild/cheat-sheets/regular-expressions/)