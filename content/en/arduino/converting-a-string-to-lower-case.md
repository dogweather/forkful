---
title:                "Converting a string to lower case"
html_title:           "Clojure recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?

Converting a string to lower case means changing all uppercase characters in a string to lowercase. Programmers often do this for consistency in data processing and comparison tasks. 

## How to:

Converting a string to lower case in Arduino is a straight-up task. Here is a simple example:

```Arduino
String message = "HELLO WORLD!";
message.toLowerCase();
Serial.println(message);
```
In this code snippet, `"HELLO WORLD!"` is transformed to `"hello world!"`. The `toLowerCase()` function does the magic.

## Deep Dive

Transforming a string to lower case isn't a new concept. It's been around for as long as the ASCII standard, defined in the 1960s, which specifically separates lowercase and uppercase characters by 32 values.

An alternative way is using the `tolower()` function in a loop for each character of the string. However, in a language like Arduino which supports high-level constructs, the `toLowerCase()` method is more efficient and easy-to-use.

The implementation of `toLowerCase()` is based on ASCII values. It scans through each character of the string and if it's an uppercase letter (ASCII value in range 65-90), it adds 32 to its ASCII value, effectively converting it to the corresponding lowercase letter.

## See Also

Check these for more insights:
- ASCII table: `http://www.asciitable.com/`
- Arduino String functions reference: `https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/`