---
title:                "C recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why

Converting a string to lower case may seem like a simple task, but it serves an important purpose in programming. By converting strings to lower case, we can ensure that the data is standardized, making it easier for us to compare and manipulate strings.

## How To

Converting a string to lower case in C requires a few simple steps. First, we need to declare a string variable and assign it a value. Then, we can use the `strlwr()` function to convert the string to lower case. Here's a code example:

```C
#include <stdio.h>
#include <string.h>

int main() {
  // Declare and initialize string
  char message[] = "Hello World";

  // Convert to lower case
  strlwr(message);

  // Print output
  printf("%s", message);
  return 0;
}
```
**Output:**

`hello world`

The `strlwr()` function is defined in the `string.h` library and accepts a string as its input. It then converts all the characters in the string to their lower case equivalents. This function is available in most C compilers and is a convenient way to achieve our goal.

## Deep Dive

For those who are interested in understanding the process behind converting a string to lower case, here's a deeper dive into the topic.

In C, strings are simply arrays of characters. Each character in the string is represented by its ASCII code, an 8-bit value that corresponds to a specific character. Upper case and lower case letters have different ASCII codes, with a difference of 32. This means that by changing the ASCII code by 32, we can convert an upper case letter to a lower case one.

The `strlwr()` function works by looping through the characters in the string and converting their ASCII codes. It also takes into account special characters, numbers, and punctuation marks, so they are not affected by the conversion.

One important thing to keep in mind when converting strings to lower case is that it is a destructive operation, meaning that it modifies the original string. If you want to keep the original string unchanged, you can create a copy of it and perform the conversion on the copy instead.

## See Also

If you want to learn more about strings and manipulating them in C, here are some helpful resources:

- [String Functions in C](https://www.programiz.com/c-programming/c-strings)
- [C - Strings](https://www.tutorialspoint.com/cprogramming/c_strings.htm)
- [C String Manipulation](https://www.geeksforgeeks.org/string-manipulation-in-c-2/#:~:text=There%20are%20various%20string%20manipulation,char%20*strcat(char%20*dest%2C)>
char%20*src))