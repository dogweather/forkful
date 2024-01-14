---
title:                "C recipe: Capitalizing a string"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why

Capitalizing a string is a common task in programming that involves changing the case of all characters in a string to uppercase. This could be useful for various reasons, such as formatting output, standardizing input, or implementing certain business logic.

## How To

In C, there are different ways to capitalize a string. The most common method is to use the `toupper()` function from the standard C library. This function takes a single character as an argument and returns its uppercase equivalent.

To capitalize a string, we need to iterate through each character of the string and call the `toupper()` function on it. Here's a simple code example:

```C
#include <stdio.h>
#include <string.h>
#include <ctype.h>

int main(){
    // Declaring and initializing a string
    char str[] = "hello world";

    // Looping through each character of the string
    for(int i = 0; i < strlen(str); i++){
        // Calling toupper() function on each character and assigning it back to the string
        str[i] = toupper(str[i]);
    }

    // Printing the capitalized string
    printf("Capitalized string: %s", str);

    return 0;
}
```

And here's the output:

```
Capitalized string: HELLO WORLD
```

Another way to capitalize a string is to use the `strlwr()` and `strupr()` functions from the `string.h` library. These functions convert a string to lowercase and uppercase, respectively. We can combine them to create a function that capitalizes a string:

```C
#include <stdio.h>
#include <string.h>

void capitalize(char str[]){
    // Converting the string to lowercase
    strlwr(str);

    // Converting the first character to uppercase
    str[0] = toupper(str[0]);
}

int main(){
    // Declaring and initializing a string
    char str[] = "hello world";

    // Calling the capitalize() function
    capitalize(str);

    // Printing the capitalized string
    printf("Capitalized string: %s", str);

    return 0;
}
```

Here's the output for this code:

```
Capitalized string: Hello world
```

## Deep Dive

In C, characters are represented by their ASCII values. The `toupper()` function takes advantage of this by converting lowercase letters (which have higher ASCII values) to uppercase letters (which have lower ASCII values) by subtracting the difference between them.

For example, the ASCII value of 'a' is 97 and the ASCII value of 'A' is 65. So, to convert 'a' to 'A', we can simply subtract 32 (97 - 32 = 65).

It's important to note that the `toupper()` function only works on lowercase letters. Any other character (such as numbers or special characters) will remain unchanged.

## See Also

- [C toupper() function documentation](https://www.tutorialspoint.com/c_standard_library/c_function_toupper.htm)
- [C string.h library documentation](https://www.tutorialspoint.com/c_standard_library/string_h.htm)
- [ASCII character set](https://www.asciitable.com/)