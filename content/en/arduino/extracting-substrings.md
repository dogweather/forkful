---
title:                "Extracting substrings"
html_title:           "Arduino recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why

Substrings are essentially smaller strings extracted from a larger string. They can be extremely useful when working with large amounts of text or data. Arduino offers a convenient way to extract substrings, making it a valuable skill for any programmer working on projects involving text manipulation.

## How To

To extract a substring in Arduino, we will use the `substring()` function. This function takes two parameters, the starting index and the length of the desired substring. Here's an example code block that shows how to extract a substring from a given string:

```Arduino
// Define a string
String fullName = "John Doe";

// Extract the first name
String firstName = fullName.substring(0, 4);

// Print the substring
Serial.println(firstName); // Outputs: John
```

In this example, we first defined a string variable called `fullName` containing the value "John Doe". Then, using the `substring()` function, we extracted the first 4 characters (starting from index 0) and stored it in a new string variable called `firstName`. Finally, we used the `Serial.println()` function to print the value of `firstName` to the serial monitor.

We can also extract substrings based on a condition. For example, if we want to extract the last name from the `fullName` variable, we can use the `indexOf()` function to find the index of the space between the first and last name, and then use that index as the starting index for our `substring()` function. Here's an example:

```Arduino
// Define a string
String fullName = "John Doe";

// Find the index of the space
int spaceIndex = fullName.indexOf(" "); // Outputs: 4

// Extract the last name
String lastName = fullName.substring(spaceIndex + 1, fullName.length());

// Print the substring
Serial.println(lastName); // Outputs: Doe
```

In this example, we first used the `indexOf()` function to find the index of the space character in the `fullName` variable. We then added 1 to that index (because we want to start from the character after the space) and used it as the starting index for our `substring()` function. We also used `fullName.length()` as the length parameter, which is the length of the original string.

## Deep Dive

The `substring()` function is a useful and convenient way to extract substrings in Arduino. However, there are a few things to keep in mind while using it. 

Firstly, the `substring()` function works only with the `String` data type in Arduino. If you are working with a character array or `char` data type, you will need to use the `strncpy()` function instead.

Secondly, the `substring()` function does not change the original string. It returns a new string with the extracted substring, so make sure to store it in a new variable if you want to use it. 

Finally, if you want to extract a substring starting from the end of the original string, you can use negative numbers as the starting index. For example, `fullName.substring(-4, fullName.length())` would extract the last 4 characters of the string.

## See Also

* [Arduino Reference](https://www.arduino.cc/reference/en/)
* [W3Schools - Arduino String Functions](https://www.w3schools.com/arduino/arduino_string_functions.asp)
* [Arduino Substring Example](https://examples.javacodegeeks.com/arduino/core/strings/arduino-substring-example/)