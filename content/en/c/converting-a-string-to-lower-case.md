---
title:                "Converting a string to lower case"
html_title:           "C recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why
One common task in programming is converting a string to lower case. This allows for easier comparison and manipulation of strings, making it a useful function to know in many applications.

## How To
```C
#include <stdio.h>
#include <string.h>
#include <ctype.h>

int main() {
  char str[50] = "Hello World";
  
  // Converting to lower case using toupper()
  for (int i = 0; i < strlen(str); i++) {
    str[i] = tolower(str[i]);
  }
  
  // Output: hello world
  printf("%s", str);
  
  return 0;
}
```
To convert a string to lower case in C, we use the `tolower()` function from the `ctype.h` library. This function takes in a character and returns its lower case equivalent. We can loop through the string and apply this function to each character to convert the whole string to lower case.

## Deep Dive
The `tolower()` function is a part of the ASCII character set, which is an encoding system that maps each character to a unique numeric value. The ASCII value for upper case letters ranges from 65 to 90, while lower case letters range from 97 to 122.

In the `for` loop in our example, we use the `strlen()` function from the `string.h` library to determine the length of the string. We then loop through each character, converting it to lower case using `tolower()` and assigning it back to the original string.

There are also other ways to convert a string to lower case in C, such as using the `strlwr()` function from the `string.h` library or the `strtolwr()` function from the `strings.h` library. However, these functions may not be available on all systems, whereas `tolower()` is a standard function that is supported by all compilers.

## See Also
- [toupper() function in C](https://www.programiz.com/c-programming/library-function/ctype.h/toupper)
- [ASCII character set](https://www.asciitable.com/)
- [string.h library in C](https://www.programiz.com/c-programming/library-function/string.h)