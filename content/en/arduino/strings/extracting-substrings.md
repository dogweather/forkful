---
title:                "Extracting substrings"
aliases: - /en/arduino/extracting-substrings.md
date:                  2024-01-20T17:44:55.544409-07:00
model:                 gpt-4-1106-preview
simple_title:         "Extracting substrings"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?

Extracting substrings means pulling out specific parts of a string - like grabbing a slice of pie. Programmers do this to isolate data, clean inputs, or manipulate text for things like parsing messages from sensors.

## How to:

Arduino strings can be sliced and diced using `substring()`:

```arduino
void setup() {
  Serial.begin(9600);
  String phrase = "Hello, Arduino World!";
  String greeting = phrase.substring(0, 5);
  String location = phrase.substring(7, 19);
  
  Serial.println(greeting); // Prints "Hello"
  Serial.println(location); // Prints "Arduino World"
}

void loop() {
  // Nothing to loop over here.
}
```

Output on Serial Monitor:
```
Hello
Arduino World
```

## Deep Dive

Way before Arduino made it simple, programmers used char arrays and functions like `strncpy` in C. Not just historical relics, they're still in use for lower-level operations. The `substring()` function in Arduino is actually a wrapper that makes it easier for us when dealing with String objects. But be aware, using `String` can lead to memory fragmentation. If stability's crucial, especially in long-running or complex programs, consider the old ways of `char` arrays.

Alternatives to `substring()` include direct char array manipulation or functions like `strtok()`. These can be more efficient but might leave you with more code to manage.

Under the hood, `substring()` creates a new String object containing the characters from the starting index to just before the ending index, which can be omitted if you want everything till the end.

## See Also:

- Arduino String Reference: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- Memory Management in Arduino: https://learn.arduino.cc/programming/variables-and-data-types/memory-management
- C++ `std::string` substr method, for comparison: http://www.cplusplus.com/reference/string/string/substr/
