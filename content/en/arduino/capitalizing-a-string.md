---
title:                "Capitalizing a string"
date:                  2024-01-19
html_title:           "C recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"

category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Capitalizing a string means making every character an uppercase letter. Programmers do this for consistency, especially in user interfaces or when preparing data for storage or comparison.

## How to:

In the Arduino environment, there isn't a built-in function to capitalize a whole string, so we'll write a simple function to do it:

```Arduino
void setup() {
  Serial.begin(9600);
  char example[] = "hello, world!";
  capitalizeString(example);
  Serial.println(example);
}

void loop() {
  // Nothing to do here
}

void capitalizeString(char* str) {
  for (int i = 0; str[i] != '\0'; i++) {
    str[i] = toupper((unsigned char)str[i]);
  }
}
```

After running the sketch, the serial monitor output shows:
```
HELLO, WORLD!
```

## Deep Dive

Historically, manipulating strings in lower-level languages like C requires working with individual characters due to the absence of high-level string manipulation functions. This tradition carries over to Arduino's C++ derivatives.

Alternatives include using `String` objects available in Arduino's C++ and calling the `.toUpperCase()` method. However, this consumes more memory. For memory-constrained environments like microcontrollers, it's often better to work with C-style character arrays (strings) and manipulate these in place.

Implementation details to remember when capitalizing a string in Arduino:
- Make sure the string is mutable (i.e., a character array).
- Use the `toupper` function from `<ctype.h>` to convert individual characters.
- String manipulation can lead to memory issues like buffer overflow if not handled carefully.

## See Also

- Arduino Reference for String `.toUpperCase()` method: https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/touppercase/
- Cplusplus.com `toupper` reference: http://www.cplusplus.com/reference/cctype/toupper/ 
- Arduino String manipulation examples: https://www.arduino.cc/en/Tutorial/BuiltInExamples/StringAdditionOperator
