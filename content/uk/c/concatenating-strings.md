---
title:                "C: З'єднання рядків"
simple_title:         "З'єднання рядків"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## Чому
Об'єднання рядків є важливим аспектом програмування в C. Воно дозволяє поєднувати різні рядки тексту для створення більш складного виразу. Це надає більш гнучкості та ефективності у програмуванні.

## Як це зробити
Для того, щоб об'єднати два рядки в C, ми можемо використовувати функцію strcat(). Ось приклад коду, який показує, як це зробити:

```C
#include <stdio.h>
#include <string.h>

int main()
{
    char str1[50] = "Привіт, ";
    char str2[50] = "C-програмісти!";

    strcat(str1, str2);

    printf("%s", str1);

    return 0;
}

```

Вихід:

Привіт, C-програмісти!

## Глибокий занурення
Функція strcat() додає рядок str2 в кінець рядка str1. Це означає, що рядок str1 буде змінений безпосередньо. Якщо ми хочемо зберегти оригінальний рядок str1, ми можемо використовувати функцію strncat(), яка дозволяє вказати максимальну кількість символів, які ви хочете додати.

Також важливо враховувати, що рядки в C мають бути завершеними нульовим символом (\0), тому перед використанням функції strcat() ми повинні переконатися, що останній символ в рядку str1 є нульовим.

## Дивитися також
- [Строки в C](https://www.tutorialspoint.com/cprogramming/c_strings.htm)
- [Функція strcat()](https://www.tutorialspoint.com/c_standard_library/c_function_strcat.htm)
- [Функція strncat()](https://www.tutorialspoint.com/c_standard_library/c_function_strncat.htm)