---
title:    "Arduino recipe: Capitalizing a string"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Why Capitalize a String in Arduino programming?

Capitalizing a string may seem like a small detail, but it can have a big impact on the readability and organization of your code. By capitalizing a string, you can easily distinguish important words or variables from the rest of your text, making it easier to spot errors and make changes.

## How To Capitalize a String in Arduino

To capitalize a string in Arduino, you can use the built-in function `toUpperCase()`. This function takes a string as a parameter and returns the same string with all letters converted to uppercase.

```Arduino
String name = "arduino";
String capitalized_name = name.toUpperCase();
Serial.println(capitalized_name); // output: ARDUINO
```

In the example above, we create a string variable called `name` with the value "arduino". Then, we use the `toUpperCase()` function to convert the string to uppercase and assign it to a new variable `capitalized_name`. Finally, we print out the value of `capitalized_name` to the serial monitor, which will display "ARDUINO".

It's important to note that the `toUpperCase()` function does not change the original string, but instead creates a new string with the capitalized letters. If you want to change the original string, you can use the `toCharArray()` function to separate the string into individual characters and then use the `toupper()` function to convert each character to uppercase.

## Deep Dive into Capitalizing a String

In Arduino, strings are actually objects of the `String` class and have many built-in functions, including the `toUpperCase()` function. This function works by looping through each character in the string and converting it to uppercase based on its ASCII code. Here is an example of the `toUpperCase()` function in action:

```Arduino
char letter = 'a';
char capitalized_letter = toupper(letter);
Serial.println(capitalized_letter); // output: A
```

As you can see, the lowercase letter "a" has an ASCII code of 97 and the uppercase letter "A" has an ASCII code of 65. The `toUpperCase()` function takes the ASCII code of a lowercase letter and subtracts 32 to get the ASCII code of the corresponding uppercase letter.

## See Also

- [Arduino String Functions Reference](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/)
- [ASCII Table](http://www.asciitable.com/)