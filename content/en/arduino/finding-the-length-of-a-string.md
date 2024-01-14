---
title:    "Arduino recipe: Finding the length of a string"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why

As an Arduino enthusiast, you might have come across situations where you need to manipulate strings. One of the most common operations is finding the length of a string, and in this blog post, we will discuss why and how to do it.

## How To

Firstly, we need to understand that a string is a sequence of characters terminated by a null character. For example, the string "Hello" would be stored as `{'H', 'e', 'l', 'l', 'o', '\0'}` in Arduino's memory. We can use a built-in function called `strlen()` to find the length of this string.

```Arduino
// Declare a string variable
String greeting = "Hello";

// Use strlen() function to find the length
int length = strlen(greeting);

// Print the result
Serial.println(length); // Output: 5
```

In the above code, we declared a string variable named "greeting" and assigned it the value "Hello". Then, we used the `strlen()` function to find the length of the string and stored the result in the "length" variable. Finally, we printed the result using the `Serial.println()` function.

Another way to find the length of a string is by using a loop and counting the characters until we reach the null character.

```Arduino
// Declare a string variable
String greeting = "Hello";

// Initialize a counter
int length = 0;

// Loop through the string until the null character is reached
while (greeting[length] != '\0') {
  length++;
}

// Print the result
Serial.println(length); // Output: 5
```

This method is useful when dealing with characters that are not a part of the ASCII character set and cannot be processed by the `strlen()` function.

## Deep Dive

It's important to note that the `strlen()` function counts the number of characters in a string, including spaces and punctuation marks. For example, the string "Hello, World!" would have a length of 13. If we want to exclude spaces and punctuation marks from the count, we need to use a more advanced method.

One way is to use the `length()` function, which is built-in the Arduino String library.

```Arduino
// Declare a string variable
String greeting = "Hello, World!";

// Use length() function to find the length
int length = greeting.length();

// Print the result
Serial.println(length); // Output: 12
```

This function only counts the visible characters in the string, excluding spaces and punctuation marks.

## See Also

- [Arduino Reference - strlen()](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/strlen/)
- [Arduino Forum - Finding String Length](https://forum.arduino.cc/index.php?topic=57948.0)
- [Arduino String Length Tutorial](https://www.instructables.com/Arduino-String-Length-Tutorial/)