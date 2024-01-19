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
String interpolation is the process of placing variable values into a string. We do this to easily concatenate string values without messing about with typical (often messy) syntax.

## How to:
Let's see a quick example in Arduino C. Note - we're assuming you've got the latest Arduino software (1.8.13 at the time of writing). 

```Arduino
String name = "Joe";
int age = 25;
String sentence = "Hello, my name is " + name + " and I am " + age + " years old.";

Serial.begin(9600);
Serial.println(sentence); // "Hello, my name is Joe and I am 25 years old."
```
Here, we've concatenated several strings and an integer.

## Deep Dive:
Historically, string formatting was quite a task, often leading to unfathomable "printf" syntax in languages like C. Languages like Python made this easier with their %.format() placeholders. Arduino, which uses a subset of C/C++, chose a simple and intuitive approach. 

As for alternatives, you could use the sprint() function or resort to manual concatenation. But remember, Arduino's string objects make things so much easier. 

Behind the scenes, when you use '+' to concatenate a string object with an integer or a float, the number is first converted to a string and then concatenated. Likewise, when two string objects are concatenated, new memory is allocated and the resultant string is placed in that new memory location. 

## See Also: 
For an in-depth view, read more about Arduino's String Class: https://www.arduino.cc/reference/control/en/language/variables/data-types/stringobject/

Curious about how string interpolation is done in other languages? Checkout: https://medium.com/@zachcaceres/string-interpolation-in-python-and-javascript-3c70fb4ffa88