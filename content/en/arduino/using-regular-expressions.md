---
title:                "Using regular expressions"
html_title:           "Bash recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?

Regular Expressions (regex) are patterns used to match character combinations in strings. Programmers use them to find, replace, or manipulate data based on specific patterns.

## How to:

Unfortunately, Arduino's standard library doesn't have native support for regex. But fear not, we've got an excellent library named `Regexp` written by Nick Gammon. Let's see it in action:

Install it from your Arduino Library Manager or GitHub.

```Arduino
#include <Regexp.h>

MatchState ms;

void setup() {
  Serial.begin(9600);
  char txt[] = "Hello Arduino 101";
  char pat[] = "[A-Za-z]{5}";

  ms.Target (txt);
  char result = ms.Match (pat);

  if (result == REGEXP_MATCHED)
    Serial.println("Match found!");
  else
    Serial.println("Match not found!");
}

void loop() {
  // nothing happens after setup
}
```
In the above example, we try to find a match for a five-letter word within the string "Hello Arduino 101". If a match exists, "Match found!" is printed.

## Deep Dive

Regex was first introduced in the early 70s with its roots in mathematical theory. It's become a go-to tool for text processing tasks due to its compactness and versatility.

The current version of Arduino doesn't provide a built-in regex functionality, which can be quite a task for beginners to grasp. But there are external libraries to showcase the magic of regex.

While using the `Regexp` library, it's worth noting that it's quite memory-hungry. Use it sparingly, especially on low-memory Arduino boards. Also, since Arduino has string manipulation shortcomings like lack of standard regular expressions, other languages like Python might be a better choice for complex manipulation tasks.

## See Also

Nick Gammon's [Regexp](http://www.gammon.com.au/Arduino/Regexp.zip) library

Learning Resources:
- [Regexr](https://regexr.com/) - Learn, build, & test Regular Expressions
- [Regular Expression Info](https://www.regular-expressions.info/tutorial.html)

Community:
- [Arduino Stack Exchange](https://arduino.stackexchange.com/questions/tagged/regular-expression) - Get help from the Arduino community
- [Arduino Project Hub](https://create.arduino.cc/projecthub?query=regular%20expressions) - Discover projects using regexp on Arduino