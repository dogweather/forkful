---
title:                "Finding the length of a string"
html_title:           "Arduino recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why
Do you ever find yourself needing to know the length of a string in your Arduino code? Maybe you want to dynamically allocate memory or perform string manipulation. Whatever the reason, knowing how to find the length of a string can be a useful skill for any Arduino programmer.

## How To
Finding the length of a string in Arduino is actually quite simple. We can use the `strlen()` function from the standard C library to do the heavy lifting for us. Let's take a look at a simple example:

```Arduino
char myString[] = "Hello World";
int stringLength = strlen(myString);
Serial.println(stringLength);
// Output: 11
```

In this code, we declare a character array `myString` and initialize it with the string "Hello World". Then, we use the `strlen()` function to find the length of the string and store it in the variable `stringLength`. Finally, we print the length to the serial monitor and get the output of 11.

But what if we have a string that is not a character array? No problem, we can simply convert it using the `c_str()` function, which returns a pointer to the underlying character array. Let's see it in action:

```Arduino
String myString = "Hello World";
int stringLength = strlen(myString.c_str());
Serial.println(stringLength);
// Output: 11
```

And just like that, we can use the `strlen()` function with both character arrays and strings.

## Deep Dive
Now, let's dive a bit deeper into how the `strlen()` function actually works. Essentially, it iterates through each character in the string until it reaches the null terminator `\0`, which marks the end of the string. It then returns the number of characters counted.

It's worth noting that the `strlen()` function is not very efficient, as it has to go through the entire string every time it is called. So if you need to find the length of a string multiple times in your code, it might be better to store the length in a variable and use that instead.

## See Also
For more information about `strlen()` and other string functions, check out the official Arduino documentation [here](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/strlen/).

And for more Arduino coding tips and tutorials, make sure to visit our website [TechGeek.com](https://www.techgeek.com/). Happy coding!