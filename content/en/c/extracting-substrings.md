---
title:                "Extracting substrings"
html_title:           "C recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why

Why would you want to extract substrings in C? Well, substrings are smaller parts of a larger string that can be useful for performing various operations such as searching, replacing, or manipulating data. By extracting substrings, you can access specific parts of a string and use them in your program.

## How To

To extract substrings in C, you will need to use the `substr()` function. This function takes three parameters: the original string, the starting index, and the length of the substring. Let's see an example:

```C
#include<stdio.h>

int main() {
    // define a string
    char str[] = "Hello World!";
    
    // extract a substring starting from index 6 with length 5
    // this will extract "World"
    char substring[6];
    substr(str, 6, 5, substring);
    
    // print the extracted substring
    printf("Substring: %s\n", substring);
    
    // output will be "Substring: World"
    
    return 0;
}
```
In this example, we have defined a string "Hello World!" and extracted a substring starting from index 6 (which is the letter "W") with a length of 5 characters, resulting in "World". Notice that we have also defined a new character array `substring` where the extracted substring will be stored. Remember to pass this array as a parameter to the `substr()` function.

To make things easier, you can also use the string library function `strncpy()` to copy the extracted substring into a character array. Let's see another example:

```C
#include<stdio.h>
#include<string.h>

int main() {
    // define a string
    char str[] = "Hello World!";
    
    // extract a substring starting from index 6 with length 5
    // this will extract "World"
    char substring[6];
    strncpy(substring, &str[6], 5);
    
    // print the extracted substring
    printf("Substring: %s\n", substring);
    
    // output will be "Substring: World"
    
    return 0;
}
```

In this example, we have used the `strncpy()` function to copy the extracted substring directly into the `substring` array. Remember to use the `&` operator before the starting index of the substring to pass it as a pointer to the function.

## Deep Dive

When extracting substrings in C, there are a few things you need to keep in mind. First, the starting index of the substring should always be within the bounds of the original string. If you try to extract a substring that goes beyond the length of the original string, you might end up with unexpected results or memory errors. 

Second, the length of the substring should also be within the bounds of the original string. Trying to extract a substring with a length larger than the remaining characters in the string will result in garbage values after the extracted substring.

Lastly, if you don't specify the length of the substring, the `substr()` function will continue extracting characters until it reaches the end of the original string. This can be useful if you want to extract the rest of the string without specifying the exact length.

## See Also

To learn more about extracting substrings in C, you can check out the following resources:

1. [The GNU C Library: String and Array Utilities](https://www.gnu.org/software/libc/manual/html_node/String-and-Array-Utilities.html)
2. [C Standard Library Tutorial: String Functions](https://www.thegeekstuff.com/2010/07/c-string-manipulation-functions/)
3. [Tutorialspoint: C - Strings](https://www.tutorialspoint.com/cprogramming/c_strings.htm)