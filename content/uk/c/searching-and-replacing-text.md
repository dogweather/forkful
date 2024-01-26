---
title:                "Пошук та заміна тексту"
date:                  2024-01-20T17:57:10.937745-07:00
model:                 gpt-4-1106-preview
simple_title:         "Пошук та заміна тексту"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?
Що таке пошук та заміна тексту та чому це роблять програмісти?

Searching and replacing text involves finding specific strings within data and substituting them with new text. Programmers do this for tasks like updating code, fixing errors, or processing user inputs.

## How to:
Як це зробити:

```C
#include <stdio.h>
#include <string.h>

void searchAndReplace(char *text, const char *search, const char *replace) {
    char buffer[1024];
    char *pos;
    int index = 0;
    int searchLen = strlen(search);

    while ((pos = strstr(text + index, search)) != NULL) {
        strncpy(buffer + index, text + index, pos - (text + index));
        strcpy(buffer + (pos - text), replace);
        index = pos - text + searchLen;
        strcpy(buffer + index, text + index);
        strcpy(text, buffer);
    }
}

int main() {
    char text[] = "Hello world! The world is beautiful.";
    const char *search = "world";
    const char *replace = "Ukraine";

    searchAndReplace(text, search, replace);

    printf("Modified text: %s\n", text);
    return 0;
}
```
Sample Output:
```
Modified text: Hello Ukraine! The Ukraine is beautiful.
```

## Deep Dive
Поглиблений аналіз:

Historical context: Text manipulation dates back to early computing, starting simple and evolving to support complex patterns with tools like regular expressions. 

Alternatives: Many languages offer built-in methods or libraries for this task—regex in Perl or JavaScript, str.replace() in Python, and std::regex in C++.

Implementation details: The code uses pointers for efficiency. It lacks buffer overflow checks for simplicity—always consider safety in real-world applications.

## See Also
Див. також:

- [C String Library Functions](https://www.tutorialspoint.com/c_standard_library/string_h.htm)
- [RegexOne – Learn Regular Expressions](https://regexone.com/)
- [GNU C Library – String and Array Utilities](https://www.gnu.org/software/libc/manual/html_node/String-and-Array-Utilities.html)
