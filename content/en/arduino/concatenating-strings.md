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

## What & Why?

Concatenating strings is the process of combining multiple strings together to create a longer string. This is a common practice in programming, as it allows developers to dynamically create strings that may change based on user input or other variables. By using concatenation, programmers can easily create more complex messages or data without having to hard code every single value.

## How to:

To use concatenation in your Arduino code, you can use the `+` operator to combine strings together. Here is an example:

```Arduino
// Combine "Hello" and "World" into a single string
String message = "Hello" + "World";

// Print out the resulting string
Serial.println(message);

// Output: HelloWorld
```

You can also concatenate variables, numbers, or other data types by converting them to strings using the `String()` function. Here's an example with a variable:

```Arduino
// Define a variable
int age = 25;

// Combine the variable with a string using concatenation
String message = "I am " + String(age) + " years old.";

// Print out the resulting string
Serial.println(message);

// Output: I am 25 years old.
```

## Deep Dive:

Concatenating strings has been a common practice in programming for a long time. In the past, strings were often stored as character arrays, and concatenating them required extra steps like manipulating pointers. But now, with languages like Arduino, concatenation is made much simpler with the use of the `String` data type and the `+` operator.

There are also alternatives to concatenating strings, such as using formatted `printf()` statements or using string functions like `strcat()` and `sprintf()`. However, for simplicity and ease of use, using concatenation is often the preferred method.

One thing to note when using concatenation is that it can use a lot of memory, so it's important to be careful when creating very long or complex strings. Additionally, be aware of any potential concatenation errors, such as missing spaces between strings or accidentally combining numbers without converting them to strings first.

## See Also:

- [Official Arduino String Reference](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [W3Schools String Concatenation Tutorial](https://www.w3schools.com/jsref/jsref_concat_string.asp)
- [C++ String Concatenation Tips](https://www.techiedelight.com/concatenate-strings-cpp/)