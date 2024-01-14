---
title:                "Arduino recipe: Concatenating strings"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why

If you're new to Arduino programming, you may have come across the concept of concatenating strings. But what does it actually mean and why would you want to use it? Simply put, concatenating strings allows you to combine multiple strings into one, which can be helpful for creating more dynamic and flexible code.

## How To

First, let's start with a basic example using the `+` operator to combine two strings:

```Arduino
String greeting = "Hello ";
String name = "John";
String message = greeting + name; 

Serial.println(message); // Output: Hello John
```

As you can see, we have combined the `greeting` and `name` strings to create a new `message` string. This can be useful if you want to personalize your message for different users.
But what if you want to include a numerical value in your string? You can use the `String()` function to convert any data type to a string, and then concatenate it with other strings. Take a look at this example:

```Arduino
int age = 25;
String name = "Jane";
String message = "Hi " + name + ", you are " + String(age) + " years old!"; 

Serial.println(message); // Output: Hi Jane, you are 25 years old!
```

You can also use the `concat()` function to concatenate multiple strings together. This can be helpful if you have a longer string that you want to add to. Let's see an example:

```Arduino
String sentence = "Today is a beautiful day.";
String ending = "Let's make the most of it!";
sentence.concat(ending);

Serial.println(sentence); // Output: Today is a beautiful day. Let's make the most of it!
```

And finally, if you want to save memory, you can use the `c_str()` function to convert your concatenated string into a character array:

```Arduino
String first = "Hello";
String last = "World";
String combined = first + last;

char result[12];
combined.toCharArray(result, 12); // 12 is the max length of the array

Serial.println(result); // Output: HelloWorld
```

## Deep Dive

It's important to note that while concatenating strings may seem simple, there are a few things to keep in mind. One is the use of the `String` data type. While it may be convenient, it can also consume a lot of memory on your Arduino board. This can become an issue if you are dealing with large amounts of data. In that case, it may be better to use character arrays instead.

Another thing to consider is the use of the `+` operator. While it works fine for smaller strings, it can become inefficient for longer strings as it creates a new string object every time it's used. This can slow down your program. So if possible, it's better to use the `concat()` function instead.

## See Also

- [Arduino Reference: String](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [Arduino Forum: String Performance](https://forum.arduino.cc/index.php?topic=395840.0)
- [Arduino Tutorial: Character Arrays](https://www.arduino.cc/en/Tutorial/CharArray)