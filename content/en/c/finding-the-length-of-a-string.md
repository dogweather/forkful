---
title:    "C recipe: Finding the length of a string"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why
As a C programmer, you may often encounter situations where you need to find the length of a string. Whether you're creating a user input validation function or manipulating strings for data processing, knowing how to find the length of a string is a crucial skill to have. In this blog post, we will explore the concept of string length and learn how to calculate it efficiently in C.

## How To
Finding the length of a string may seem like a simple task, but it requires a good understanding of C string functions. The most commonly used method is by using the `strlen()` function, which counts the number of characters in a string. Let's take a look at an example:

```C
#include <stdio.h>
#include <string.h>

int main() {
    char str[] = "Hello World";
    int len = strlen(str);
    printf("The length of the string is %d\n", len);
    return 0;
}

```
Output:
```
The length of the string is 11
```
In the above example, we first declared a string variable `str` and initialized it with the string "Hello World". Then, we used the `strlen()` function to calculate the length of the string and stored it in an `int` variable `len`. Finally, we printed the length using `printf()` function. 

Another method of finding the length of a string is by using a loop and the `NULL` character. The `NULL` character marks the end of a string in C, so by looping through the string and incrementing a counter until `NULL` is encountered, we can find the length of the string. Take a look at the following code:

```C
#include <stdio.h>

int main() {
    char str[] = "Hello World";
    int len = 0;
    while (str[len] != '\0') {
        len++;
    }
    printf("The length of the string is %d\n", len);
    return 0;
}
```
Output:
```
The length of the string is 11
```
In this example, we first declared a string variable `str` and initialized it with the string "Hello World". Then, we used a `while` loop to iterate through the string and incremented the `len` variable until the `NULL` character was encountered. Finally, we printed the length using `printf()` function. 

## Deep Dive
Now, let's take a deeper dive and understand how the `strlen()` function works under the hood. The `string.h` header file in C contains the declaration of `strlen()` function as:

`size_t strlen(const char *str);`

The `size_t` type is an unsigned integer type used for storing the size of an object. The function takes a `const` pointer to a character as an argument and returns the length of the string. 

The `strlen()` function uses a pointer to traverse the string and counts the number of characters until the `NULL` character is encountered. It is a simple and efficient way of calculating the length of a string.

## See Also
For more information on string functions in C, check out these resources:
- [C Strings and String Library Functions](https://www.geeksforgeeks.org/c-strings-and-string-library-functions/)
- [The Standard Library - <string.h>](https://www.gnu.org/software/libc/manual/html_node/String-and-Array-Utilities.html#index-strlen-1057)

By now, you should have a good understanding of how to find the length of a string in C. Remember, having a good grasp of string functions is essential for efficient string manipulation, so keep practicing and exploring different methods. Happy coding!