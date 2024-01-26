---
title:                "Визначення довжини рядка"
date:                  2024-01-20T17:47:10.184031-07:00
model:                 gpt-4-1106-preview
simple_title:         "Визначення довжини рядка"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why? / Що і Чому?
Finding the length of a string means figuring out how many characters it contains. Programmers need this to manipulate text data efficiently, like when processing user input or displaying messages.

## How to: / Як це зробити:
Let's dive right into the code. Below, you'll see a simple way to find a string's length using the `strlen` function from the C Standard Library.

```c
#include <stdio.h>
#include <string.h>

int main() {
    char myString[] = "Привіт, світ!";
    size_t length = strlen(myString);

    printf("The length of the string is: %zu\n", length);

    return 0;
}
```
Sample output:
```
The length of the string is: 13
```
Note: The length doesn't count the null terminator `\0`, which indicates the end of the string.

## Deep Dive / Поглиблене Занурення:
In C, strings are arrays of `char` types, ended with a null terminator. Historically, this was designed for simplicity and efficiency in a time when memory was scarce.

While `strlen` is standard, there are caveats. It counts characters, not glyphs or grapheme clusters, which means for some languages (like where diacritics are used), the perceived length may differ from `strlen` output.

Another classic method is a `for` loop:
```c
size_t myStrlen(char *string) {
    size_t length = 0;
    while(string[length] != '\0') {
        length++;
    }
    return length;
}
```

What's more, C11 (ISO/IEC 9899:2011) introduced more secure string functions like `strnlen_s`, which requires a maximum length and can avoid buffer overruns.

## See Also / Дивіться також:
- C11 Standard (ISO/IEC 9899:2011): https://www.iso.org/standard/57853.html
- C String handling: http://www.cplusplus.com/reference/cstring/
- Unicode Strings and C: http://www.unicode.org/reports/tr20/tr20-5.html

Remember, these functions are your bread and butter when working with strings in C; understanding how to swiftly and accurately gauge their lengths is crucial. Keep experimenting and investigating better ways to manage and understand text. Your journey with C strings is just beginning. Happy coding!
