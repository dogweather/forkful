---
title:    "Arduino recipe: Concatenating strings"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Why

Concatenating strings in Arduino programming allows for combining multiple strings into one, making it easier to display and manipulate text in your projects. It also helps to reduce the number of variables needed, making your code more efficient.

## How To

To concatenate strings in Arduino, follow these steps:

1. Create two variables, each containing a different string:

```Arduino
String greeting = "Hello, ";
String name = "Arduino";
```

2. Use the `+` operator to merge the two strings together:

```Arduino
String message = greeting + name;
```

3. Use the `print()` function to display the concatenated string:

```Arduino
Serial.print(message);
```

4. To add additional text or variables, simply use `+` between them:

```Arduino
String message = greeting + name + "!";
```

The output would be: "Hello, Arduino!"

## Deep Dive

Concatenation in Arduino uses the `String` object, which is a special data type that allows for storing and manipulating text. This is different from the traditional C programming language, which uses null-terminated character arrays for strings.

When using `String` in Arduino, the strings are treated as objects, which means they have built-in functions for manipulating them. This includes concatenating strings using the `+` operator.

It is important to note that repeated concatenation operations on a `String` object can cause memory fragmentation, which can lead to instability in your code. To avoid this, it is recommended to use the `String.reserve()` function to allocate memory for the final concatenated string.

Another thing to keep in mind is that the `String` object has a limited capacity, so you may encounter issues if you try to concatenate very large strings.

## See Also

- [Arduino String concatenation reference](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/concat/)
- [Tutorial on concatenating strings in Arduino](https://www.tutorialspoint.com/arduino/arduino_string_concatenation.htm)
- [Explanation of strings and String object in Arduino](https://www.makerguides.com/arduino-string-array/)