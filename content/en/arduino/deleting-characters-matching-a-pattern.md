---
title:                "Arduino recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why

Arduino programming is all about creating unique and functional projects with just a few lines of code. One common challenge that Arduino users may encounter is the need to delete characters matching a pattern. But why would someone want to do this? Well, this could be useful when working with data from sensors or user input, where unwanted characters may interfere with the desired functionality of the project.

## How To

Luckily, Arduino provides us with a simple and efficient way to delete characters matching a pattern. Let's take a look at the following example, where we want to delete all the commas from a string:

```
Arduino sketch
String data = "34,82,56,12";
data.replace(",", "");
Serial.println(data);
```

In this code, we first declare a string variable called "data" and assign it a value of "34,82,56,12". Then, we use the `replace()` function to replace all the commas in the string with an empty character, effectively deleting them. Finally, we print the updated string to the serial monitor using `Serial.println()`. 

The output of this code will be: "34825612", with all the commas successfully deleted. You can use this method to delete any character or pattern from a string in your Arduino projects.

## Deep Dive

There are two key components that make the `replace()` function work: the character or pattern to be replaced and the replacement character. In our example, the pattern was "," and the replacement character was an empty string. However, we can also use this function to replace a single character with another character. For example:

```
Arduino sketch
String word = "Hello!";
word.replace("!", "?");
Serial.println(word);
```

The output of this code will be "Hello?" with the exclamation mark replaced by a question mark.

It's also important to note that the `replace()` function only replaces the first occurrence of the character or pattern in the string. So, if you have multiple occurrences of the pattern, you'll need to use a loop to replace all of them.

## See Also

- Learn more about string manipulation in Arduino: https://www.arduino.cc/reference/en/language/variables/data-types/string/manipulation

- Check out other useful string functions in Arduino: https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/

- Get creative and try out different patterns and replacement characters in your projects!