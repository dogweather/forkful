---
title:                "Searching and replacing text"
html_title:           "Arduino recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?

Searching and replacing text is a common task in programming where specific words or phrases in a given text are replaced with new ones. This is often done to make code more organized, fix spelling errors, or update text according to changing requirements. It is a useful tool for programmers to efficiently maintain and improve their code.

## How to:

To search and replace text in Arduino, we can use the ```replace()``` function. Its syntax is ```replace(stringToModify, target, replacement)```, where ```stringToModify``` is the text that needs to be changed, ```target``` is the word or phrase to be replaced, and ```replacement``` is the new word or phrase to be inserted.

Here's an example of how to use it:

```
#include <string.h>

void setup() {
    Serial.begin(9600);

    // Original text
    String text = "Hello, world!";

    Serial.println("Before replace:");
    Serial.println(text);

    // Replaced text
    replace(text, "Hello", "Hi");

    Serial.println("After replace:");
    Serial.println(text);
}

void loop() {

}
```

### Output:

```
Before replace:
Hello, world!
After replace:
Hi, world!
```

As you can see, the ```replace()``` function changes the original string to the new one. This can be extended to more complex text manipulation by using loops and conditional statements.

## Deep Dive

The ```replace()``` function was first introduced in the Arduino programming language in version 1.0. It is based on the standard ```String``` library, which provides various string manipulation functions. However, some programmers argue that using the ```String``` library can cause memory issues and recommend using C-style strings instead.

Alternatives to the ```replace()``` function include using regular expressions, which are powerful tools for pattern matching and replacing in strings. Arduino also has a built-in function called ```strreplace()``` which can replace characters or substrings in a string.

## See Also

- [Arduino String Reference](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replace/)
- [Regular Expressions in Arduino](https://www.arduino.cc/reference/en/language/structures/strings/regular-expressions/)
- [C-style Strings vs Arduino String Library](https://arduino.stackexchange.com/questions/61183/difference-between-c-style-strings-and-arduino-strings)