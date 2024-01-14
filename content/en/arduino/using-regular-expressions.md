---
title:    "Arduino recipe: Using regular expressions"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Why

Regular expressions are an essential tool for any programmer, including those working with Arduino. They allow you to efficiently search and manipulate strings of data, making your code more powerful and versatile. Whether you're working on a project or tinkering with code for fun, learning how to use regular expressions can greatly enhance your programming skills.

## How To

To use regular expressions in your Arduino code, you will need to include the Arduino Regular Expressions library. Once you have the library installed, you can begin using regular expressions in your code.

Let's say you have a string of data from a sensor that looks like this:

```
int sensorData = "12.768,40.554"
```

You want to extract the two numbers from this string. This is where regular expressions come in handy. You can use the `Regex` library to define a pattern and then extract the matching data from the string.

```
#include <Regex.h>

// Define the pattern to search for
Regex pattern("([0-9]+\.[0-9]+),([0-9]+\.[0-9]+)");

// Use the match() function to find matches in the string
Match match = pattern.match(sensorData);

// Print out the matching data
Serial.println(match.group(1)); // outputs 12.768
Serial.println(match.group(2)); // outputs 40.554
```

As you can see, regular expressions allow you to specify patterns to match in a string and extract the relevant data. This makes it easier and more efficient to process strings of data in your code.

## Deep Dive

Regular expressions use a combination of symbols and characters to define a pattern. Some common symbols include:

- `.` (dot) - matches any single character
- `*` (asterisk) - matches the preceding element zero or more times
- `+` (plus) - matches the preceding element one or more times
- `?` (question mark) - matches the preceding element zero or one time
- `|` (pipe) - matches either the element on the left or the element on the right

You can also use brackets to specify a range of characters. For example, `[0-9]` would match any single digit from 0 to 9.

There are many other symbols and characters you can use in regular expressions, so it's worth exploring and experimenting to become comfortable with them. Regular expressions can be tricky at first, but with practice, you'll soon be able to use them to solve complex string manipulation problems.

## See Also

- [Arduino Regular Expressions Library](https://github.com/shirriff/Arduino-regexp)
- [Regex Tutorial](https://regexone.com/)
- [Regex Cheat Sheet](https://cheatography.com/davechild/cheat-sheets/regular-expressions/)