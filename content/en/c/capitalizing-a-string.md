---
title:    "C recipe: Capitalizing a string"
keywords: ["C"]
---

{{< edit_this_page >}}

## Why
Capitalizing a string is a common task in programming, especially when dealing with user input or manipulating data. It allows for consistency and organization in the output of a program. 

## How To
To capitalize a string in C, there are a few steps to follow. First, we need to include the `string.h` library for string functions. Then, we need to define a string that we want to capitalize. After that, we can use the `toupper()` function to convert each character in the string to uppercase. Finally, we print out the capitalized string to see the desired result.

```C
#include <stdio.h>
#include <string.h>

int main(){
    char str[] = "hello world";
    int i;
    
    // convert each character to uppercase
    for(i = 0; i < strlen(str); i++){
        str[i] = toupper(str[i]);
    }
    
    // print out the capitalized string
    printf("%s", str);
    return 0;
}
```

The output of this program would be `HELLO WORLD`, with all letters capitalized. This method can also be used for single characters or specific parts of a string. For more advanced manipulations, we can use functions like `strcmp()` to compare strings or `strcpy()` to copy strings.

## Deep Dive
In C, each character is represented by a numerical value known as its ASCII code. The `toupper()` function takes in a character and returns its uppercase ASCII code. This is why we can use a loop to convert each character in a string to uppercase. It is important to note that this function only works for letters, and will not affect other characters such as numbers or symbols.

It is also worth mentioning that there are alternative methods for capitalizing strings in C, such as using bitwise operators or creating a new function to do the job. However, the `toupper()` function is the simplest and most commonly used method.

## See Also
- [C string functions](https://www.tutorialspoint.com/c_standard_library/string_h.htm)
- [ASCII table](https://www.asciitable.com/)
- [C bitwise operators](https://www.programiz.com/c-programming/bitwise-operators)