---
title:                "Searching and replacing text"
html_title:           "Arduino recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?

Searching and replacing text is exactly what it sounds like – locating snippets of text within a string or a file and changing them into something else. This task is key in various programming scenarios, from simple data cleaning to complex data manipulation techniques. 

## How to:

Here's a simple, straightforward code example in C to demonstrate the replace operation.

```C
#include<stdio.h>
#include<string.h>

void replace(char * str, char oldCh, char newCh) {
    for(int i = 0; i<strlen(str); i++) {
        if(str[i] == oldCh) str[i] = newCh;
    }
}

int main() {
    char str[] = "Hello, World!";
    replace(str, 'o', '0');
    printf("%s\n", str);

    return 0;
}
```
In the sample code above, we've implemented a simple replace function that replaces every occurrence of an old character with a new one. We then test this function with the string "Hello, World!" – replacing 'o' with '0'.

When you run this C program, the following output is produced:
```
Hell0, W0rld!
```
## Deep Dive

Historically, replacement in strings relates closely to the development of text editors and the desire to automate tasks. Sed, or the Stream EDitor, introduced in the early Unix days, was one of the first programs to do search and replace. 

In modern programming, there are several alternatives for string replacement in C. These include using built-in functions like `strncpy()` or `memmove()`, or using third-party libraries like regex, which provide more options for string manipulation.

The basic implementation of replace, like the one we've given above, has a time complexity of O(n), since we're looping over each character of the string. But replacing a substring with another requires a time complexity of O(n*m) where m is the size of the original string and n is the size of the substring we're replacing. Remember to consider time complexity when choosing your implementation approach!

## See Also 

1. C library function - **[strncpy()](https://www.tutorialspoint.com/c_standard_library/c_function_strncpy.htm)**

2. C library function - **[memmove()](https://www.tutorialspoint.com/c_standard_library/c_function_memmove.htm)**

3. Info on time complexity of algorithms - **[Big-O Notation](https://www.interviewcake.com/article/java/big-o-notation-time-and-space-complexity)**

4. Unix's Sed command - **[Sed Stream Editor](https://www.gnu.org/software/sed/manual/sed.html)**

Exploring these resources will support your understanding and ability to implement searching and replacing text in C. Happy Coding!