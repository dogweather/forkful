---
title:    "C recipe: Converting a string to lower case"
keywords: ["C"]
---

{{< edit_this_page >}}

## Why
In programming, strings are a common way to represent text data. However, depending on the application, it may be necessary to convert all characters in a string to lower case. This can be useful for tasks such as sorting or data processing. In this blog post, we will explore how to convert a string to lower case in the C programming language.

## How To
First, let's take a look at the code for converting a string to lower case in C.

```
#include <stdio.h>
#include <string.h>
#include <ctype.h>

int main() {
  char string[] = "Hello World!";
  
  // Convert to lower case
  for (int i = 0; i < strlen(string); i++) {
    string[i] = tolower(string[i]);
  }
  
  printf("%s", string);
  return 0;
}
```

In the above code, we first include the necessary header files for string manipulation and character conversion. Next, we declare a string variable and initialize it with the text "Hello World!". Then, we use a for loop to iterate through each character in the string and use the `tolower()` function from the `ctype.h` library to convert it to lower case. Finally, we print the modified string to the console.

The output of this code will be:

```
hello world!
```

Simple, right? Now that we have a basic understanding of the code, let's dive a little deeper into how it works.

## Deep Dive
In C, characters are represented by their ASCII codes, which are numeric values assigned to each character. The `tolower()` function takes in an ASCII code and returns the corresponding lower case character. So when we pass in the ASCII code for the character 'H', which is 72, the function returns 104, which is the ASCII code for 'h'. This process is repeated for each character in the string, effectively converting it to lower case.

It is important to note that this method will only work for strings that are composed solely of alphabetic characters. If the string contains other characters such as numbers or symbols, they will not be affected by the conversion.

There are also other methods for converting a string to lower case, such as using the `strlwr()` function from the `string.h` library. However, it is not recommended to use this function as it is not considered safe due to its potential to cause buffer overflows.

## See Also
- Wikipedia: [ASCII](https://en.wikipedia.org/wiki/ASCII)
- GeeksforGeeks: [String Character Conversions in C/C++](https://www.geeksforgeeks.org/c-programming-string-character-conversions/)

In conclusion, converting a string to lower case in C can be achieved with a simple for loop and the `tolower()` function. It is a useful technique to know when manipulating text data in a program. Thanks for reading!