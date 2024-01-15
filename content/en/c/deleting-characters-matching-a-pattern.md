---
title:                "Deleting characters matching a pattern"
html_title:           "C recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why
There may be situations where a programmer needs to delete certain characters from a string based on a specific pattern. This can help with data cleanup or formatting operations, making the code more efficient and readable.

## How To
To begin, we need to understand the basic syntax for deleting characters from a string in C. The function `strchr()` can be used to locate a specific character in a string. It takes two arguments: the string to search and the character to find. Here's an example:
```C
char str[] = "Hello World";
char *ptr = strchr(str, 'l'); // finds the first occurrence of 'l' in the string
```
To delete multiple characters, we can use a for loop and check each character against our desired pattern. Here's an example that deletes all lowercase letters from a string:
```C
char str[] = "Hello World";
for(int i = 0; i < strlen(str); i++) {
    if(str[i] >= 'a' && str[i] <= 'z') { // checks if the character is lowercase
        memmove(&str[i], &str[i+1], strlen(str) - i); // moves all remaining characters one index to the left
        i--; // decrease i to check the new character at the current index
    }
}
```
The resulting string will be "H W" as all lowercases have been deleted. Keep in mind that `memmove()` is used instead of `strcpy()` to ensure that the null character at the end of the string is also moved.

## Deep Dive
Instead of using the `strchr()` and `memmove()` functions, we can also use pointers to achieve the same result. The `strchr()` function returns a pointer to the first occurrence of the character in the given string. Similarly, the `memmove()` function also uses pointers to manipulate the string. Here's an example of how we can delete all spaces in a string using pointers:
```C
char str[] = "Hello World";
char *ptr = str;
while(*ptr) { // loops until the null character is reached
    if(*ptr == ' ') { // checks if the character is a space
        char *temp = ptr;
        while(*temp) { // moves all characters after 'ptr' one index to the left
            *temp = *(temp + 1);
            temp++;
        }
        continue; // continues to next iteration without increasing ptr
    }
    ptr++; // increase ptr to check the next character
}
```
This will effectively delete all spaces from the string and the resulting string will be "HelloWorld".

## See Also
- [C String Functions](https://www.programiz.com/c-programming/c-string-functions)
- [C Pointer Tutorial](https://www.tutorialspoint.com/cprogramming/c_pointers.htm)