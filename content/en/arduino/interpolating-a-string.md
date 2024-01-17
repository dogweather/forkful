---
title:                "Interpolating a string"
html_title:           "Arduino recipe: Interpolating a string"
simple_title:         "Interpolating a string"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Interpolating a string is the process of inserting variables or data into a specified string. This allows for a more dynamic and efficient way of assigning values to strings. Programmers use string interpolation to avoid concatenation, which can be cumbersome and less readable.

## How to:

```Arduino
// Basic example of string interpolation in Arduino

int num = 10;
String name = "John";

Serial.println("Hello, my name is " + name + " and I am " + num + " years old.");
// Output: Hello, my name is John and I am 10 years old.
```

In the above example, the values for `name` and `num` are inserted into the specified string using the `+` operator. This allows for a much cleaner and readable code compared to concatenation.

## Deep Dive:

String interpolation is not a new concept and has been used in programming languages such as Python and Ruby for a long time. In Arduino, it was first introduced in version 1.0.5 to simplify string manipulation. Prior to this, concatenation was used to achieve string interpolation which resulted in longer and more complicated code.

An alternative to string interpolation in Arduino is using the `String()` function, which allows for variables to be converted to strings. However, this method can be less efficient and more error-prone compared to string interpolation.

In terms of implementation, string interpolation in Arduino is achieved using the `+` operator. Internally, this operator uses the `String()` function to convert the variables to strings and then combines them. This makes string interpolation a more efficient method compared to concatenation.

## See Also:

- [Arduino String Interpolation Documentation](https://www.arduino.cc/reference/en/language/variables/data-types/string/interpolation/)
- [Arduino String Function Reference](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/)
- [Blog post on String Interpolation in Arduino](https://blog.arduino.cc/2016/10/12/a-look-at-string-interpolation-in-arduino/)