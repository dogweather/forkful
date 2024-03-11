---
date: 2024-02-03 19:02:45.377922-07:00
description: "Regular expressions (regex) are sequences of characters that define\
  \ search patterns, primarily used for string matching and manipulation. Programmers\u2026"
lastmod: '2024-03-11T00:14:34.181288-06:00'
model: gpt-4-0125-preview
summary: "Regular expressions (regex) are sequences of characters that define search\
  \ patterns, primarily used for string matching and manipulation. Programmers\u2026"
title: Using regular expressions
---

{{< edit_this_page >}}

## What & Why?
Regular expressions (regex) are sequences of characters that define search patterns, primarily used for string matching and manipulation. Programmers leverage regex in Arduino projects for parsing serial inputs, validating user input, or extracting data from strings, enhancing data processing efficiency and flexibility.

## How to:
Arduino does not have built-in support for regex directly in its standard library. However, you can achieve regex-like functionality for simple patterns using basic string functions, or for more complex needs, integrate a third-party library such as `regex`.

### Basic String Matching without Regex
For basic needs, like finding a substring, you can use the `String.indexOf()` function:
```cpp
String data = "Sensor value: 12345";
int index = data.indexOf("value:");
if (index != -1) {
  String value = data.substring(index + 6).trim();
  Serial.println(value); // Outputs: 12345
}
```

### Using a Third-Party Library for Regex
To handle more complex patterns, you might consider a library like `regex`. After installing the library, you can use it as follows:

1. **Installation**: The `regex` library might not be readily available in the Arduino Library Manager, so you may need to manually install it by downloading from a reputable source and adding it to your Arduino libraries folder.

2. **Example Usage**:
Assuming the library provides functionalities similar to standard regex implementations, you might use it as follows:

```cpp
#include <regex.h>

void setup() {
  Serial.begin(9600);
  while (!Serial); // Wait for Serial to be ready
  
  regex_t reg;
  const char* pattern = "[0-9]+"; // Matches a sequence of digits
  regcomp(&reg, pattern, REG_EXTENDED);
  
  const char* test_str = "Sensor value: 12345";
  
  regmatch_t matches[1];
  if (regexec(&reg, test_str, 1, matches, 0) == 0) {
    // Extract and print the matching portion
    int start = matches[0].rm_so;
    int end = matches[0].rm_eo;
    char match[end-start+1];
    strncpy(match, test_str + start, end-start);
    match[end-start] = '\0';
    
    Serial.print("Found match: ");
    Serial.println(match); // Outputs: 12345
  } else {
    Serial.println("No match found");
  }
  
  regfree(&reg); // Free the allocated memory for regex
}

void loop() {
  // put your main code here, to run repeatedly:
}
```

**Note**: The syntax and specific functions used here are for illustrative purposes and might vary based on the actual implementation details of the `regex` library you choose. Always refer to the library's documentation for accurate and up-to-date information.
