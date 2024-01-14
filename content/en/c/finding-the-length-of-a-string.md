---
title:    "C recipe: Finding the length of a string"
keywords: ["C"]
---

{{< edit_this_page >}}

## Why

String manipulation is a fundamental concept in programming, and one common task is to find the length of a string. Understanding how to find the length of a string can greatly improve your skills as a programmer and make your code more efficient.

## How To

Finding the length of a string in C is a simple task, but it requires some knowledge of how strings are represented in the language. In C, strings are simply arrays of characters, with a null character (\0) at the end to mark the end of the string.

To find the length of a string, we can use the `strlen()` function from the standard library. This function takes in a string as a parameter and returns the length of the string. Let's look at an example:

```C
#include <stdio.h>
#include <string.h>

int main() {
    char myString[] = "Hello!"; // declare a string
    int length = strlen(myString); // find the length using strlen()
    printf("The length of the string is: %d\n", length);
    return 0;
}
```
Output:
```
The length of the string is: 6
```

We can also calculate the length of a string manually using a loop. Let's see how to do this:

```C
#include <stdio.h>

int main() {
    char myString[] = "Hello!";
    int length = 0;
    
    // loop through the string until we reach the null character
    while(myString[length] != '\0') {
        length++;
    }
    
    printf("The length of the string is: %d\n", length);
    return 0;
}
```
Output:
```
The length of the string is: 6
```

## Deep Dive

The `strlen()` function may seem like a simple magic tool, but it actually works by counting the number of characters until it reaches the null character. This means that the function has a time complexity of O(n), where n is the length of the string.

If you want to make your code more efficient, you can also consider implementing your own function to calculate the length of a string. This can be done using a loop, as shown in the second example above, or by using pointer arithmetic. However, keep in mind that the `strlen()` function from the standard library is optimized for different systems and may still be the most efficient option.

## See Also

- [C String](https://en.wikibooks.org/wiki/C_Programming/Strings)
- [The C Programming Language](https://www.amazon.com/Programming-Language-2nd-Brian-Kernighan/dp/0131103628)
- [Arrays in C](https://www.geeksforgeeks.org/arrays-in-c-cpp/)