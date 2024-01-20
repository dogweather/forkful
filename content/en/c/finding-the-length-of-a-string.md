---
title:                "Finding the length of a string"
html_title:           "Arduino recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Finding the length of a string means determining the number of characters a string holds. Programmers use this measure in various operations such as loops, comparisons and when allocating memory dynamically for strings.

## How to:
Let's use the `strlen()` function in `<string.h>` library to find the length of a string.

```C
#include <stdio.h>
#include <string.h>

int main() {
    char str[] = "Hello, Programmer!";
    int len = strlen(str);
    printf("Length of string = %d\n", len);
    return 0;
}
```
This outputs: `Length of string = 18`. It includes all characters and spaces but doesn't count the concluding null character.

## Deep Dive
Finding the length of a string might seem like a simplistic task, but there's quite a history behind it. In older versions of C, there was no built-in function to achieve this. Programmers needed to loop through the string, counting the characters until a null character (`\0`) was encountered. This is the end-of-string marker in C.

Here's the manual way without `strlen()`:

```C
#include <stdio.h>

int main () {
    char str[] = "Hello, Programmer!";
    int len = 0;
    while (str[len] != '\0') {
        len++;
    }
    printf("Length of string = %d\n", len);
    return 0;
}
```
This outputs: `Length of string = 18`, matching the earlier result. Notice how the code's complexity increased.

The `strlen()` function in the `<string.h>` library was a welcome upgrade. Internally, it loops through the string till it finds a null character, exactly simulating the manual process, but the usage becomes more readable and neat.

## See Also
To continue your journey into string manipulation in C, here are some useful links:
- [C String Functions](https://www.tutorialspoint.com/c_standard_library/string_h.htm) on TutorialsPoint.