---
title:                "Удаление кавычек из строки"
date:                  2024-01-29T00:01:37.236803-07:00
model:                 gpt-4-0125-preview
simple_title:         "Удаление кавычек из строки"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/c/removing-quotes-from-a-string.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?

Удаление кавычек из строки означает извлечение любых знаков кавычек — будь то одинарные ('') или двойные ("") — которые являются частью содержимого строки. Программисты делают это для санитизации ввода, подготовки данных к дальнейшей обработке или избегания синтаксических ошибок при работе с путями к файлам и командами в языках, которые используют кавычки для обозначения строк.

## Как:

Вот функция C, которая очистит ваши строки от этих назойливых кавычек:

```c
#include <stdio.h>
#include <string.h>

void remove_quotes(char *str) {
    char *p_read = str, *p_write = str;
    while (*p_read) {
        if (*p_read != '"' && *p_read != '\'') {
            *p_write++ = *p_read;
        }
        p_read++;
    }
    *p_write = '\0';
}

int main() {
    char str[] = "He said, \"Hello, 'world'!\"";
    printf("Исходный: %s\n", str);
    remove_quotes(str);
    printf("Очищенный: %s\n", str);
    return 0;
}
```

Пример вывода:

```
Исходный: He said, "Hello, 'world'!"
Очищенный: He said, Hello, world!
```

## Подробнее

Удаление кавычек из строки является задачей с зари программирования, где гигиена данных была и остается ключом к избежанию ошибок (например, атак SQL инъекций) или важна для того, чтобы строку можно было безопасно передать системам, которые могут путать кавычку с управляющим символом.

Исторически, разные языки по-разному справляются с этой задачей — некоторые имеют встроенные функции (например, `strip` в Python), в то время как другие, такие как C, требуют ручной реализации из-за их ориентации на предоставление разработчикам контроля на более низком уровне.

Альтернативы включают использование библиотечных функций, таких как `strpbrk` для поиска кавычек, или использование регулярных выражений (с библиотеками, такими как PCRE) для более сложных шаблонов, хотя это может быть излишним для простого удаления кавычек.

Приведенная выше реализация просто сканирует каждый символ в строке, копируя только символы без кавычек в место положения указателя записи. Это эффективно, поскольку выполняется на месте без необходимости дополнительной памяти для результирующей строки.

## Смотрите также

- [Функции стандартной библиотеки C](http://www.cplusplus.com/reference/clibrary/)
- [PCRE - Perl Compatible Regular Expressions](https://www.pcre.org/)
- [Понимание указателей в C](https://www.learn-c.org/en/Pointers)