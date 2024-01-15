---
title:                "Finding the length of a string"
html_title:           "C recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why

If you're new to the world of programming, you may be wondering why on earth someone would need to know the length of a string. Well, it's a crucial piece of information that allows you to manipulate and work with strings, which are essentially just sequences of characters. Whether you're creating a text-based game or parsing through data, being able to find the length of a string is a valuable skill to have.

## How To

To find the length of a string in C, all you need is the `strlen()` function from the string.h library. Let's take a look at an example:

```C
#include <stdio.h>
#include <string.h>

int main() {
    char string[] = "Hello world!";
    
    printf("The length of the string '%s' is %d.", string, strlen(string));
    return 0;
}
```

Output: The length of the string 'Hello world!' is 12.

The `strlen()` function takes in a string as an argument and returns an integer representing the length of the string. It works by counting the number of characters in the string until it reaches a null terminator, denoted by a `\0` at the end of the string.

## Deep Dive

Now, let's take a deeper dive into how the `strlen()` function works. First, it initializes a variable to hold the length of the string as 0. Then, it begins looping through each character of the string until it reaches the null terminator. On each iteration, it increments the length variable by 1. Once it reaches the null terminator, it returns the final length value. It may seem simple, but this function is an essential and efficient way to find the length of any string.

## See Also

If you're interested in learning more about string manipulation in C, here are some helpful resources:

- [Official C Documentation for Strings](https://en.cppreference.com/w/c/string)
- [GeeksforGeeks: Strings in C](https://www.geeksforgeeks.org/strings-in-c-2/)
- [TutorialsPoint: C - Strings](https://www.tutorialspoint.com/cprogramming/c_strings.htm)