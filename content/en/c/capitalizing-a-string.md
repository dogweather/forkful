---
title:                "C recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why

Have you ever wanted to capitalize a string in your C program? Maybe you're working on a project that requires certain text to be in all caps, or you just prefer the aesthetic of capital letters. In any case, knowing how to capitalize a string can be a useful skill to have in your programming repertoire.

## How To

In C, strings are simply arrays of characters, with the last character being a null terminator. So to capitalize a string, we need to access each character, convert it to uppercase if it is a lowercase letter, and then reassign it back to the string.

Let's take a look at a simple example:

```C
#include <stdio.h>
#include <ctype.h>
#include <string.h>

int main() {
  char str[] = "hello world";
  
  // loop through each character in the string
  for (int i = 0; i < strlen(str); i++) {
    // check if the current character is a lowercase letter
    if (islower(str[i])) {
      // convert it to uppercase using toupper() function
      str[i] = toupper(str[i]);
    }
  }
  
  // print the capitalized string
  printf("%s\n", str);
  
  return 0;
}
```

Output: `HELLO WORLD`

We first include the necessary header files for string manipulation and character operations. Then, we define a string `str` with the value "hello world". Using a for loop, we iterate through each character in the string and check if it is a lowercase letter using the `islower()` function. If it is, we convert it to uppercase using `toupper()` and reassign it back to the string.

## Deep Dive

Now, let's take a deeper look at how this code works. In C, each character is represented by a numerical value based on the ASCII table. Uppercase letters have lower numerical values compared to lowercase letters. For example, 'A' has a value of 65 while 'a' has a value of 97. So by subtracting 32 from the lowercase value, we get the corresponding uppercase value. This is essentially what the `toupper()` function does.

We use `strlen()` to determine the length of the string and use it as the condition for our loop. The `islower()` function checks if a character is a lowercase letter and returns a non-zero value if it is. This serves as the condition for our `if` statement, where we convert the lowercase character to uppercase and reassign it back to the string.

## See Also

To learn more about string manipulation in C, check out these helpful resources:

- [String Manipulation in C Tutorial](https://www.programiz.com/c-programming/c-strings)
- [C Programming Language](https://en.wikipedia.org/wiki/C_(programming_language))
- [C Standard Library: <ctype.h>](https://www.tutorialspoint.com/c_standard_library/ctype_h.htm)