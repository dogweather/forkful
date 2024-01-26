---
title:                "Перетворення рядка у нижній регістр"
date:                  2024-01-20T17:37:50.513942-07:00
model:                 gpt-4-1106-preview
simple_title:         "Перетворення рядка у нижній регістр"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?
## Що та Навіщо?

String to lower case conversion means transforming all letters in a string to their lowercase equivalent. Programmers use it for consistency, case-insensitive comparisons, and data normalization.

## How to:
## Як це зробити:

Here's a simple C function to convert a string to lower case:

```C
#include <stdio.h>
#include <ctype.h>

void toLowerCase(char *str) {
    for (int i = 0; str[i]; i++) {
        str[i] = tolower(str[i]);
    }
}

int main() {
    char myString[] = "Hello, World!";
    toLowerCase(myString);
    printf("%s\n", myString); // Output will be: hello, world!
    return 0;
}
```

## Deep Dive
## Поглиблений Розгляд

In the early days, computers did not have a standard for character encoding. ASCII became a de facto way to represent English characters. `tolower()` in C is a legacy of this, helping navigate case differences effortlessly.

Alternatives include manually checking each character and converting using character arithmetic, for example `str[i] += 'a' - 'A';`, assuming the character is uppercase. However, this is error-prone and does not cover languages beyond English.

Implementation details: `tolower()` comes from `<ctype.h>` and handles conversion according to the current locale setting. This allows it to work with different character sets. It's reliable, well-tested, and portable.

## See Also
## Дивись Також

- C Standard Library reference: https://en.cppreference.com/w/c/string/byte/tolower
- Unicode and character sets: https://unicode.org
- ASCII table and description: http://www.asciitable.com/
