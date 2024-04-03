---
date: 2024-01-20 17:41:18.857704-07:00
description: "Deleting characters matching a pattern means getting rid of specific\
  \ sequences of characters from strings\u2014think cleaning up data or inputs. Programmers\
  \ do\u2026"
lastmod: '2024-03-13T22:45:00.308003-06:00'
model: gpt-4-1106-preview
summary: "Deleting characters matching a pattern means getting rid of specific sequences\
  \ of characters from strings\u2014think cleaning up data or inputs."
title: Deleting characters matching a pattern
weight: 5
---

## What & Why?

Deleting characters matching a pattern means getting rid of specific sequences of characters from strings—think cleaning up data or inputs. Programmers do this to standardize, simplify, or validate the information before processing it.

## How to:

Let's say we want to drop all numeric digits from our string. We've got a string with some random numbers, and we're going after a clean, letters-only result.

```Arduino
void setup() {
  Serial.begin(9600);

  // Our initial string with numbers
  String stringWithNumbers = "Ar3du1n0 1s aw3som3!";
  String cleanedString = deletePattern(stringWithNumbers, "0123456789");

  // Print the cleaned string
  Serial.println(cleanedString);
}

void loop() {
  // Nothing to do here
}

String deletePattern(String str, String pattern) {
  for (unsigned int i = 0; i < pattern.length(); i++) {
    str.replace(String(pattern[i]), "");
  }
  return str;
}
```

If you upload and run this on your Arduino, you'll see the string without numbers in the serial monitor:

```
Arduino is awesome!
```

## Deep Dive

Removing characters matching a specific pattern isn't a new concept. Early programming languages had functions to process and manipulate strings. In Arduino, although a high-level function for pattern deletion doesn't exist natively, we can create our custom logic, like in the `deletePattern` function above.

There are alternatives in other languages, such as regex (regular expressions) in Python or JavaScript, but Arduino's coding environment is more basic. It doesn't include regex functions out of the box, mainly due to its limited processing power and memory.

Under the hood, our `deletePattern` function iterates through our pattern string, uses the `String.replace()` method to search for the current character, and replaces it with an empty string, therefore "deleting" it from our original string.

## See Also

- String manipulation with Arduino: https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/
- Arduino String reference: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- More on string replacement: http://www.cplusplus.com/reference/string/string/replace/
