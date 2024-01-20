---
title:                "Конкатенація рядків"
html_title:           "PHP: Конкатенація рядків"
simple_title:         "Конкатенація рядків"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## Що і чому?
Конкатенація рядків - це процес з'єднання двох або більше рядків в один. Програмісти цим користуються для того, щоб створювати та маніпулювати рядками даних в коді.

## Як це робити:
Нижче надані приклади коду та результати їх виконання у блоках коду C ```C ... ```

```C
#include <stdio.h>
#include <string.h>

int main() { 
    char str1[50] = "Добрий ";
    char str2[50] = "день!";

    strcat(str1, str2);

    printf("Результат: %s", str1);

    return 0;
}
```
Коли ви запускаєте цю програму, ви отримуєте:
```C
Результат: Добрий день!
```

## Більш глибокий розбір
Конкатенація рядків у програмуванні використовується вже довгий час. Вона використовується для створення динамічних рядків або для об'єднання інформації.

Як альтернатива strcat(), ви можете використати strncat(), яка дозволяє вказати максимальну кількість символів, які треба скопіювати:

```C
strncat(char *dest, const char * src, size_t n)
```
Під час конкатенації рядків важливо знати, що у другий рядок додається символ кінця рядка ('\0'). Це є важливим, бо воно вказує на кінець рядка в мові C. Крім того, потрібно переконатися, що в кінцевому рядку достатньо місця для зберігання об'єднаних рядків.

## Дивіться також

1. [Стрічки в C - GeeksforGeeks](https://www.geeksforgeeks.org/strings-in-c-2/)
2. [C Library Functions - Tutorialspoint](https://www.tutorialspoint.com/c_standard_library/index.htm)
3. [C String Concatenation: strncat() - Programiz](https://www.programiz.com/c-programming/string-concatenation)