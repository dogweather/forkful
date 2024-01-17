---
title:                "Перетворення рядка на нижній регістр"
html_title:           "C: Перетворення рядка на нижній регістр"
simple_title:         "Перетворення рядка на нижній регістр"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Що і чому?

Конвертування рядка в нижній регістр є процесом перетворення всіх літер рядка з великого регістру в малий. Це корисне для програмістів, які хочуть зробити зрозумілий рядок або порівняти рядки без врахування регістру.

## Як?

```C
#include <stdio.h>
#include <string.h>

int main() {
    char str[] = "HELLO WORLD";
    for (int i = 0; str[i] != '\0'; i++) {
        if (str[i] >= 'A' && str[i] <= 'Z') {
            str[i] = str[i] + 32; // примітка: ASCII значення великої літери більше на 32
        }
    }
    printf("%s", str); // виведе "hello world"
    return 0;
}
```

## Глибока занурення

Цей процес знайшов своє походження ще в 1960-х роках, коли букви були записані на дисках у верхньому регістрі. В наші часи існують інші способи роблення рядків малими літерами, наприклад, використання функції tolower() з бібліотеки ctype.h.

## Дивіться також

- [Функція tolower()](https://www.tutorialspoint.com/c_standard_library/c_function_tolower.html)
- [ASCII таблиця](https://www.asciitable.com/)