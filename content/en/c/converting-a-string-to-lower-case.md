---
title:                "Converting a string to lower case"
html_title:           "C recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?

Converting a string to lower case is a common task in programming where a string is converted from containing uppercase letters to lowercase letters. This is often done to standardize input or to compare strings without worrying about the case. By converting to lower case, programmers can save time and avoid potential mistakes in their code.

## How to:

To convert a string to lower case in C, we can use the `tolower()` function from the standard library. It takes in a character as an argument and returns its lowercase equivalent. Here's an example:

```C
#include <stdio.h>
#include <ctype.h>

int main() {
    char str[] = "HeLlo WoRLd!";
    for (int i = 0; str[i] != '\0'; i++) {
        str[i] = tolower(str[i]);
    }
    printf("%s", str);
}
```

Output:
```
hello world!
```

We can also use the `tolower()` function with strings stored in arrays or pointers. Here's another example:

```C
#include <stdio.h>
#include <ctype.h>

int main() {
    char *str = "ByE Bye";
    while (*str != '\0') { 
        *str = tolower(*str);
        str++;
    }
    printf("%s", str);
}
```

Output:
```
bye bye
```

## Deep Dive:

Historically, converting strings to lower case was done by hand using ASCII codes to determine the lowercase equivalent of each character. However, this is prone to errors and can be time-consuming. Hence, the `tolower()` function was introduced in the C programming language to make this task easier and more efficient.

An alternative to using `tolower()` is the `strlwr()` function, but it is not part of the standard library and is only available in certain compilers. It also has some limitations, such as not converting extended ASCII characters.

In terms of implementation, the `tolower()` function can be implemented using bitwise operations to convert the ASCII code of a character to its lowercase equivalent. This is more efficient than using a series of conditional statements.

## See Also:

- [ASCII code](https://www.asciitable.com/)
- [C library functions](https://www.ibm.com/support/knowledgecenter/en/ssw_ibm_i_74/rtref/clibfunc.htm) for more string manipulation functions.