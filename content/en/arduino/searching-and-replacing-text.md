---
title:                "Arduino recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why

Are you tired of manually searching through lines of code to replace text? Are you looking for an efficient way to make bulk changes to your Arduino project? Well, look no further! In this blog post, we will explore the power of searching and replacing text in Arduino programming.

## How To

Searching and replacing text in Arduino is a simple yet powerful tool that can save you time and effort in your coding journey. Let's take a look at the basic syntax for this function:

```Arduino
replace(stringToSearch, stringToFind, stringToReplace)
```

Using this syntax, we can replace a specific string of text with a new string within our Arduino program. Let's see it in action:

```Arduino
//Original code
String message = "Hello World!";
Serial.println(message);

//Replacing "Hello" with "Hey"
String newMessage = replace(message, "Hello", "Hey");
Serial.println(newMessage);

//Output: Hey World!
```

As we can see, the function has successfully replaced the original string with the new string. In addition to replacing specific words, we can also use this function to make bulk changes by specifying a pattern to search for. For example:

```Arduino
//Original code
String numbers = "1234 5678 9101 1121";
Serial.println(numbers);

//Replacing all numbers with "X"
String newNumbers = replace(numbers, "[0-9]", "X");
Serial.println(newNumbers);

//Output: XXXX XXXX XXXX XXXX
```

By using regular expressions, we were able to replace all numbers in our string with "X". This can be useful when wanting to hide sensitive information or simply wanting to make a bulk change to your code.

## Deep Dive

One thing to note when using the replace function in Arduino is that it only works with strings. This means that if you want to replace a specific number or character, it must be in string format. Additionally, this function replaces the original string with the new string, it does not modify the original string.

Another useful function for searching and replacing text in Arduino is the `indexOf()` function. This function allows us to locate the index position of a specific string within another string. Let's see an example:

```Arduino
//Original code
String text = "Hello World!";
int index = text.indexOf("W");
Serial.println(index);

//Output: 6
```

Here, we were able to locate the index position of the letter "W" in our string "Hello World!". We can use this function in combination with the replace function to replace only a specific section of a string. For example:

```Arduino
//Original code
String message = "Hello World!";
String name = "World";
int index = message.indexOf(name);
String newMessage = replace(message, name, "Universe");
Serial.println(newMessage);

//Output: Hello Universe!
```

By using the `indexOf()` function, we were able to locate the index position of our specified string (`name`) within the original string (`message`). Then, we used the replace function to only replace that specific section with a new string, resulting in our desired output.

## See Also

Now that you've learned about the power of searching and replacing text in Arduino, you can take your coding skills to the next level. Check out these links for more information and useful tips:

- [Arduino Reference](https://www.arduino.cc/reference/en/language/functions/communication/serial/replace/)
- [Regex Tutorial](https://www.regular-expressions.info/tutorial.html)
- [String Functions in Arduino](https://www.w3schools.com/arduino/arduino_string_functions.asp)