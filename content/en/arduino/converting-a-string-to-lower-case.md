---
title:    "Arduino recipe: Converting a string to lower case"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why

Have you ever encountered a situation where you needed to convert a string to lower case while programming an Arduino? Maybe you're receiving data from a sensor that sends out all capital letters and you need to manipulate it for your project. Whatever the reason may be, knowing how to convert a string to lower case in Arduino can come in handy in various projects.

## How To

Converting a string to lower case in Arduino is a simple process that can be achieved using the `toLowerCase()` function. Let's take a look at an example:

```Arduino
String str = "HELLO WORLD";
str = str.toLowerCase(); // converts the string to "hello world"
Serial.println(str); // outputs "hello world" to the serial monitor
```

In this example, we declare a `String` variable called `str` with the value "HELLO WORLD". Then, using the `toLowerCase()` function, we convert the string to lower case and assign it back to the `str` variable. Finally, we print the string to the serial monitor to see the output.

Another way to convert a string to lower case in Arduino is by using the `toLowerCase()` function from the `String` class. Here's an example using this method:

```Arduino
String str = "HELLO WORLD";
String lowerStr = str.toLowerCase(); // converts the string to "hello world" and stores it in a new variable
Serial.println(lowerStr); // outputs "hello world" to the serial monitor
```

As you can see, the `toLowerCase()` function can be used on both a `String` variable and directly on a `String` value. This makes it a versatile tool for converting strings to lower case in your Arduino projects.

## Deep Dive

Behind the scenes, the `toLowerCase()` function works by using the ASCII value of each character in the string. In the ASCII table, capital letters have a higher value than lower case letters. So, by adding a specific number to the ASCII value, the `toLowerCase()` function can convert a capital letter to its lower case equivalent.

For example, the ASCII value of "A" is 65, and the ASCII value of "a" is 97. So, to convert "A" to "a", the `toLowerCase()` function adds 32 to the ASCII value. This process is repeated for every character in the string, resulting in a converted string with all lower case letters.

Additionally, the `toLowerCase()` function only works with ASCII characters, meaning that any non-ASCII characters (such as accented letters) will not be converted.

## See Also

- [Arduino String class reference](https://www.arduino.cc/reference/en/language/variables/data-types/stringclass/)
- [ASCII table](https://www.ascii-code.com/)
- [Converting characters in C++](https://www.tutorialspoint.com/convert-rtn-to-lower-case-in-cplusplus)

Converting a string to lower case may seem like a small task, but it can be a useful skill to have in your Arduino programming toolkit. With the `toLowerCase()` function, you can easily manipulate strings and data to fit the needs of your project. So go ahead and try it out in your next project!