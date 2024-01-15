---
title:                "Concatenating strings"
html_title:           "Arduino recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why

So you've been working on your Arduino project and everything is going smoothly, until you realize you need to combine multiple strings together. But why bother concatenating strings in the first place? Well, sometimes it's necessary to display a message or build a URL with dynamic values. Whatever the reason may be, concatenating strings can come in handy when programming with Arduino.

## How To

Concatenating strings in Arduino is a relatively simple task. First, make sure you have the string library included at the top of your code:

```Arduino
#include <string.h>
```

Next, declare your strings and assign them values:

```Arduino
String name = "John";
String message = "Hello, my name is " + name + "!";
```

Notice how the strings are combined using the `+` operator. Now, if you were to print out the `message` variable, the output would be:

```
Hello, my name is John!
```

You can also concatenate integers and other data types by converting them to strings using the `String()` function:

```Arduino
int age = 25;
String bio = "I am " + String(age) + " years old.";
```

The `bio` variable would now hold the value of:

```
I am 25 years old.
```

## Deep Dive

Concatenating strings in Arduino works similarly to how it works in other programming languages like C or Java. The `+` operator is used to combine strings, and the `+=` operator can be used to append strings to an existing variable.

However, it's important to note that you should not use the `+` operator repeatedly in a loop to concatenate strings. This can cause memory overflow and your program may crash. Instead, use the `concat()` function to combine strings within a loop.

Another thing to keep in mind is that strings in Arduino are mutable, meaning you can change their values. This can be useful when you need to update dynamic values in your concatenated string.

## See Also

- [Arduino Official Documentation on Strings](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [Concatenating Strings in C - Tutorialspoint](https://www.tutorialspoint.com/cprogramming/c_strings.htm)
- [Understanding Memory in Arduino - Adafruit](https://learn.adafruit.com/memories-of-an-arduino/understanding-memory)