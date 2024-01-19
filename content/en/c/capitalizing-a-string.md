---
title:                "Capitalizing a string"
html_title:           "C recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Capitalizing a string refers to converting its first letter to uppercase. Programmers often use it for formatting, such as when a sentence starts with that string or to follow title case conventions in UI display.

## How to:

Here are some code snippets to capitalize a string:

```C
#include <stdio.h> 
#include <ctype.h> 

void capitalize(char* str) 
{ 
   // If the first char in the string is lower case  
    if(islower(str[0])) 
    { 
        // change it to upper case 
        str[0] = toupper(str[0]); 
    } 

    printf("Capitalized string: %s", str); 
} 

int main() 
{ 
   char str[] = "hello world!";
   capitalize(str);
   return 0; 
} 
```

This snippet will output:

```C
Capitalized string: Hello world!
```

## Deep Dive:

- Historical context: Since the inception of C programming, string manipulation, including capitalization, has been a common practice. Early use cases stem from the need to standardize input data, enhance readability, and follow linguistic and lexical rules. 
- Alternatives: Apart from the function used above (`toupper`), we could use other standard library functions like `strlwr` and `strupr` for full lower and upper case conversion respectively. Note that these aren't part of the standard C library but are often provided by compilers.
- Implementation details: `toupper` in the code snippet converts a single character to upper case if it is in lower case; it leaves the character unchanged if it's either in upper case already or it's not an alphabetic character. This function deals with single characters, not strings, the reason why it's applied to the first character of the string only.

## See Also:

- [More on String Manipulation in C](http://www.learn-c.org/en/Strings)
- [C Standard Library ctype.h](https://www.cplusplus.com/reference/cctype/)
- [C Function Implementation Details](https://en.wikipedia.org/wiki/C_standard_library)