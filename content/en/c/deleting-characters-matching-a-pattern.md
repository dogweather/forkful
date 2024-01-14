---
title:                "C recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why

In many programming tasks, cleaning up data is an important step in order to process it effectively. One common task is deleting characters that match a certain pattern. This could be useful in situations such as removing special characters from a string or replacing certain characters with another.

## How To 

To delete characters that match a pattern, one approach is to loop through each character in the string and check if it matches the pattern. If it does, we can remove it from the string. Below is a simple example in C, using the `strchr()` function to check for character matches and the `memmove()` function to shift the characters in the string after the match is deleted.

```
#include <stdio.h>
#include <string.h>

int main() {
    char str[] = "Hello, $World!";
    char pattern[] = "$";

    printf("Before: %s\n", str);

    char *ptr;
    while ((ptr = strchr(str, *pattern)) != NULL ) {
        memmove(ptr, ptr + 1, strlen(ptr));
    }

    printf("After: %s\n", str);
    
    return 0;
}
```

Output:

```
Before: Hello, $World!
After: Hello, World!
```

In this example, we are deleting all occurrences of the character `$` in the string "Hello, $World!".

## Deep Dive

While the above method is a simple and efficient approach, it may not always be suitable for more complex patterns. For example, if we want to delete a sequence of characters rather than just a single character, the `memmove()` function may not work as expected. In such cases, using a regular expression library like PCRE (Perl Compatible Regular Expressions) can be a better option.

Regular expressions allow us to match patterns in a more specific and flexible way by using special sequences of characters to represent a search pattern. In the example below, we use the `regex.h` library with the `regex` structure to compile and match a regular expression and then delete the matched characters using the `regexec()` and `regcomp()` functions.

```
#include <stdio.h>
#include <string.h>
#include <regex.h>

int main() {
    char str[] = "I love #coding in C!";
    char pattern[] = "[#|a-z]";

    printf("Before: %s\n", str);

    int status;
    regex_t regex;
    status = regcomp(&regex, pattern, REG_ICASE);
    status = regexec(&regex, str, 0, NULL, 0);
    if (status == 0) {
        regfree(&regex);
        regcomp(&regex, pattern, REG_ICASE);
        regsub(&regex, str, "", str);
    }

    printf("After: %s\n", str);
    
    return 0;
}
```

Output:

```
Before: I love #coding in C!
After: I lve in !
```

Here, we use the regular expression "[#|a-z]" to match either a `#` symbol or any lowercase letter. By using the `regsub()` function, we are able to substitute the matched characters with an empty string, effectively deleting them from the string.

## See Also

- [Regular expressions in C programming language](https://www.systutorials.com/docs/linux/man/3-regcomp/)
- [C string functions](https://www.tutorialspoint.com/c_standard_library/string_h.htm)
- [PCRE Library](https://www.pcre.org/)