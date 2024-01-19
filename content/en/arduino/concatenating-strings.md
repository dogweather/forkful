---
title:                "Concatenating strings"
html_title:           "PHP recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?

Concatenating strings means joining two or more strings together. It's a common task for handling and manipulating data in programming.

## How to:

In Arduino, you can concatenate strings using the `+` or `+=` operator. Here's a simple recipe:

```Arduino
String hello = "Hello";
String world = "World";
String greeting = hello + ", " + world;
Serial.println(greeting);
```
Output: `Hello, World`

You can also append, or concatenate, using `+=`. It joins the new string to the end of the existing string. Like so:

```Arduino
String greeting = "Hello";
greeting += ", ";
greeting += "World";
Serial.println(greeting);
```
Output: `Hello, World`

## Deep Dive

Historically, `"char"` arrays were used in C/C++ for handling string data. But juggling them was a chore. Arduino introduced the `String` class to simplify this. 

However, using `String` comes with a cost - dynamic memory allocation. It can lead to memory fragmentation. For uber-large applications or long run times, consider `char` arrays or the `strcat()` function for lower-level control.

Alternatives? Sure! In fact, you can use `sprintf` to concatenate and format strings:

```Arduino
char greeting[20];
sprintf(greeting, "Hello, %s", "World");
Serial.println(greeting); 
```
Output: `Hello, World`

This gives you lower-level control, avoids memory fragmentation but is somewhat trickier to handle.

## See Also 

1. [Arduino String Reference](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
2. [Arduino Print class reference i.e., println()](https://www.arduino.cc/en/Serial/Print)
3. [StackOverflow discussion about String performance issues](https://arduino.stackexchange.com/questions/44936/arduino-string-versus-char-array)