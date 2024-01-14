---
title:    "Arduino recipe: Converting a string to lower case"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Why

Converting a string to lower case can be a useful tool when working with strings in your Arduino programming. It allows you to standardize your inputs and compare strings with ease, making your code more efficient and organized.

## How To

To convert a string to lower case in Arduino, you can use the `toLowerCase()` function. This function takes in a string as the input and returns a lower case version of that string. Here is a simple example of how to use it:

```Arduino
String original = "HELLO";
Serial.println(original);  // Output: HELLO
String lowercase = original.toLowerCase();
Serial.println(lowercase);  // Output: hello
```

As you can see, the `toLowerCase()` function converts all the letters in the original string to lower case. It is important to note that this function does not change the original string, but rather creates a new string with the lower case version.

You can also use the `toLowerCase()` function in a comparison. For example:

```Arduino
String answer = "yes";
if (answer.toLowerCase() == "yes") {
  Serial.println("This will be printed!");
}
```

In this case, even if the user inputs "YES" or "yEs", the `toLowerCase()` function will convert it to "yes" and the condition will be satisfied.

## Deep Dive

When converting a string to lower case, it is important to consider the language being used. Some languages have special characters that are not affected by the `toLowerCase()` function. For example, in German, the letter "ß" becomes "ss" in lower case, but the `toLowerCase()` function will not convert it. In this case, you may need to manually replace the "ß" with "ss" before using the `toLowerCase()` function.

It is also worth noting that the `toLowerCase()` function only works with ASCII characters. If your string contains non-ASCII characters, they will not be converted to lower case. In this case, you may need to use a different function or manually convert the characters to lower case.

## See Also

Here are some helpful resources to learn more about converting strings to lower case in Arduino:

- [Arduino Language Reference - Strings](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [Converting Strings to Lower Case in Arduino](https://www.instructables.com/Converting-Strings-to-Lower-Case-in-Arduino/)
- [ASCII Table](https://www.asciitable.com/)