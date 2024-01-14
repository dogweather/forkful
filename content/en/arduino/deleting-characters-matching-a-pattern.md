---
title:    "Arduino recipe: Deleting characters matching a pattern"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Why
As Arduino enthusiasts, we often encounter situations where we need to manipulate strings and characters in our code. One common task is deleting characters that match a certain pattern. But why would someone want to do this?

Well, here are a few reasons- maybe you want to get rid of unnecessary symbols or punctuation marks from a string, or you want to filter out certain characters before performing a specific action. Whatever the reason may be, knowing how to delete characters matching a pattern can come in handy while programming with Arduino.

## How To
To delete characters matching a pattern, we can utilize the `remove()` function in Arduino. This function takes two parameters- the starting index and the number of characters to be removed. Here's an example code block showing how we can use this function to delete all vowels from a string:

```Arduino
// Initializing the string
String message = "Hello Arduino!";
// Deleting characters matching the pattern "aeiou"
message.remove(1, 5); // The first argument is the starting index and the second is the number of characters to remove
Serial.println(message); // Output: Hll Ardno!
```

In the above example, we used the `remove()` function to delete the characters at index 1 to 5 (which are the vowels in this case). This is just one way to delete characters matching a pattern, and you can tweak the starting index and number of characters accordingly to achieve your desired result.

## Deep Dive
Now, let's dive a bit deeper into the `remove()` function and understand how it actually works. When we call this function, it internally uses another function called `setCharAt()` which updates the value of the character at a specific index with a new character.

For example, in the above code, when we called `remove(1, 5)`, it first called `setCharAt(1, 'l')`, `setCharAt(2, 'l')`, and so on until it reached the end of the string. This effectively replaces the vowels with the value 'l', thus deleting them from the string.

It's important to note that the `remove()` function modifies the original string itself and doesn't return a new string. So, if you want to keep the original string intact, make a copy of it before performing any modifications.

## See Also
To learn more about the `remove()` function and other useful string manipulation functions in Arduino, check out these resources:

- [Arduino Reference - String](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [Arduino String Manipulation and Formatting](https://www.arduino.cc/en/Tutorial/StringExamples)
- [Getting Rid of Unwanted Characters - Arduino Forum](https://forum.arduino.cc/index.php?topic=486095.0)

With these resources and the knowledge of how to delete characters matching a pattern, you can now successfully manipulate strings and characters to suit your needs in Arduino. Happy coding!