---
date: 2024-01-20 17:37:34.108743-07:00
description: "Converting a string to lower case changes all uppercase letters in the\
  \ text to their lowercase counterparts. Programmers do this for consistency,\u2026"
lastmod: '2024-02-25T18:49:56.748385-07:00'
model: gpt-4-1106-preview
summary: "Converting a string to lower case changes all uppercase letters in the text\
  \ to their lowercase counterparts. Programmers do this for consistency,\u2026"
title: Converting a string to lower case
---

{{< edit_this_page >}}

## What & Why?
Converting a string to lower case changes all uppercase letters in the text to their lowercase counterparts. Programmers do this for consistency, especially when comparing strings or standardizing input data.

## How to:
Arduino's `String` object has a handy `toLowerCase()` method. Call it on your string, and just like that, it's in lowercase.

```Arduino
void setup() {
  Serial.begin(9600);
  String message = "Hello, World!";
  message.toLowerCase();
  Serial.println(message);  // Outputs: hello, world!
}

void loop() {
  // Nothing to do here.
}
```
Fire up your Serial Monitor, and you'll see "hello, world!" printed out.

## Deep Dive
Historically, dealing with text often involved accounting for upper and lower case. Data entry, search, and sort operations typically ignore case to reduce user error and increase robustness. In other languages, like C, you'd iterate over each character and convert them individually using standard library functions. In Arduino land, `String` objects wrap this functionality for ease of use.

Alternatives? Sure. You might use `toLowerCase()` for a `char` array, but you'll have to walk through each character and convert it with `tolower()` from `<ctype.h>`. If you're concerned about memory and performance, consider using character arrays over `String` objects and take control with your custom lowercasing logic.

## See Also
- Arduino's `String` reference page: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- C++ `<cctype>` library for character operations: http://www.cplusplus.com/reference/cctype/
- For an understanding of how string comparison works and why ignoring case can be important, check out: https://en.wikipedia.org/wiki/String_(computer_science)#Comparison
