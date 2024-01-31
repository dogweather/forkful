---
title:                "Перетворення рядка на великі літери"
date:                  2024-01-19
simple_title:         "Перетворення рядка на великі літери"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? / Що таке і навіщо?

Capitalize a string means converting the first letter of each word to uppercase, while keeping the rest lowercase. Programmers use it to normalize data input, for readability, or to meet coding standards.

## How to: / Як це зробити:

In Arduino, you handle strings with the `String` object. Here's how you can capitalize words:

```Arduino
String capitalize(String input) {
  String output = "";
  bool capitalizeNext = true;

  for (int i = 0; i < input.length(); i++) {
    if (capitalizeNext && isLowerCase(input[i])) {
      output += (char)(input[i] - 32); // convert to uppercase
    } else {
      output += input[i];
    }
    capitalizeNext = isspace(input[i]); // check for space
  }
  return output;
}

void setup() {
  Serial.begin(9600);
  String myString = "hello world"; // String to capitalize
  Serial.println(capitalize(myString));
}

void loop() {
  // Nothing to do here
}
```

Sample output:
```
Hello World
```

## Deep Dive / Поглиблене занурення:

Capitalizing strings isn't new. It's been around since the early days of computing. In C, you'd iterate over each character and use ASCII values to transform lowercase to uppercase. Arduino's `String` makes it a bit easier.

Alternatives? Sure. You could use `charAt()` and `setCharAt()` to modify the string in place. But this is simpler and cleaner.

As for implementation details, remember that ASCII value of 'a' is 97, and 'A' is 65. The difference is 32, which is why you see `input[i] - 32` in the code.

## See Also / Дивіться також:

- Arduino `String` reference: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- ASCII table for understanding character conversions: https://www.asciitable.com/
- More on C/C++ strings for background knowledge: http://www.cplusplus.com/reference/string/string/
