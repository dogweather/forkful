---
title:                "Великі літери в рядках"
html_title:           "C: Великі літери в рядках"
simple_title:         "Великі літери в рядках"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Що & Чому?

Великі літери в рядку означають перетворення всіх символів рядка на великі літери. Програмісти це роблять, щоб полегшити порівняння і сортування рядків або підкреслити певну інформацію в тексті.

## Як це зробити:

Ось деякі прості приклади того, як виконати це в C:

```C
#include <ctype.h>
#include <stdio.h>

void capitalize(char* str) {
    for (int i = 0; str[i]!='\0'; i++){
        str[i] = toupper(str[i]);
    }
}

int main() {
    char str[] = "programming in c";
    capitalize(str);
    printf("%s\n", str);

    return 0;
}
```

Output:

```C
PROGRAMMING IN C
```

## Поглиблений огляд:

Historically, C has not had built-in string manipulation functions like some high-level languages. You could, of course, write your own functions to convert strings to uppercase or lowercase, but this would be time-consuming and prone to errors. Therefore, functions like `toupper()` were born, found in `<ctype.h>`, to facilitate the transformation of string case.

Alternatively, you can manually convert each character to uppercase by taking advantage of the ASCII representation of characters. This isn't recommended, though, because it's more error-prone and less portable.

## Дивіться також:

For more on the `toupper()` function: https://en.cppreference.com/w/c/string/byte/toupper

For alternatives to `toupper()`: stackoverflow.com/questions/26696718/do-i-really-need-toupper-to-make-a-string-uppercase-in-c
For a full guide on string manipulation in C: www.tutorialspoint.com/c_standard_library/c_function_toupper.htm