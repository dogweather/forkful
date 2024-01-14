---
title:                "C recipe: Extracting substrings"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why

Substrings, or smaller parts of a larger string, can be incredibly useful in programming. They allow us to manipulate and analyze specific portions of data instead of dealing with the entire string as a whole. Extracting substrings can be especially handy when working with user input or parsing through data. 

## How To

To extract substrings in C, we will be using the `strncpy()` function. This function takes in three parameters: the destination string, the source string, and the number of characters to be copied. Let's take a look at an example:

```
#include <stdio.h>
#include <string.h>

int main()
{
    char source[] = "Hello World";
    char destination[6];
    
    strncpy(destination, source, 5);
    
    printf("Extracted substring: %s", destination);
    
    return 0;
}
```

In this code, we first declare a source string "Hello World" and a destination string with enough space to hold 5 characters. Then, we use `strncpy()` to copy the first 5 characters from the source string into the destination string. Finally, we print out the destination string to see our extracted substring. The output of this code will be "Hello".

## Deep Dive

While `strncpy()` is a useful function for extracting substrings, it does have its limitations. One major issue is that it doesn't automatically add a null terminator to the end of the extracted substring. This means that if you try to print out the extracted substring without adding a null terminator manually, it may print out additional characters from the source string.

Another limitation is that `strncpy()` doesn't handle overlapping strings well. If the source and destination strings overlap, the results may not be what you expect.

To overcome these limitations, we can use the `strndup()` function instead. This function takes in two parameters: the source string and the number of characters to be copied. Unlike `strncpy()`, `strndup()` automatically adds a null terminator at the end of the extracted substring and can handle overlapping strings.

## See Also

- [C String Library Reference](https://www.tutorialspoint.com/c_standard_library/string_h.htm)
- [Basic Strings in C](https://www.programiz.com/c-programming/c-strings)