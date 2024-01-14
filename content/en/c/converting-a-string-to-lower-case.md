---
title:    "C recipe: Converting a string to lower case"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why
There are various reasons why someone may need to convert a string to lower case in C programming. One common scenario is when working with user input, as it ensures that the input is in a consistent format for manipulation and comparison purposes.

## How To
In C programming, the standard library function `tolower()` can be used to convert a character to lower case. However, this function only works on a single character at a time. To convert an entire string to lower case, we can use a loop to iterate through each character and apply the `tolower()` function. Here is an example:

```C
#include <stdio.h>
#include <ctype.h>

int main() {
  char string[] = "CoNvErT mE TO lOwEr CaSe!";
  int i;
  
  for (i = 0; string[i] != '\0'; i++) { // Loop through each character
    string[i] = tolower(string[i]); // Convert to lower case
  }
  
  printf("%s", string); // Output the converted string
  return 0;
}
```

### Sample Output:
`convert me to lower case!`

## Deep Dive
In C programming, strings are represented as arrays of characters. Each character is stored in a sequential memory location. This means that by manipulating the characters in the array, we can change the entire string. 

In the example above, we used the `tolower()` function from the `ctype.h` library. This function takes in a character as an argument and returns the lowercase equivalent. It does this by checking the ASCII value of the character and subtracting 32 from it if it is an uppercase letter (since the ASCII values for uppercase letters are 32 more than their lowercase counterparts).

It is important to note that the `tolower()` function only works for the standard ASCII characters. If you are working with non-ASCII character sets, you may need to use a different method for converting to lower case.

## See Also
- [ASCII character set](https://en.wikipedia.org/wiki/ASCII)
- [ctype.h library](https://www.cplusplus.com/reference/cctype/)
- [Converting strings to upper case in C](https://www.geeksforgeeks.org/convert-string-uppercase-keeping-string-unchanged/)