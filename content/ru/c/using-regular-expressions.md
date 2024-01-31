---
title:                "Использование регулярных выражений"
date:                  2024-01-29T00:04:32.649464-07:00
model:                 gpt-4-0125-preview
simple_title:         "Использование регулярных выражений"

category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/c/using-regular-expressions.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?
Регулярные выражения (regex) используются для поиска, сопоставления и манипуляции строками. Программисты используют их для проверки текста, поиска и преобразования, ускоряя задачи обработки текста.

## Как:
В C нет встроенной поддержки регулярных выражений, но можно использовать библиотеки, такие как `regex.h`. Вот простой пример сопоставления с образцом.

```c
#include <stdio.h>
#include <regex.h>

int main() {
    regex_t regex;
    int result;
    char *pattern = "^hello";
    char *text = "hello world";

    // Компиляция регулярного выражения
    result = regcomp(&regex, pattern, REG_EXTENDED);

    if (result) {
        printf("Не удалось скомпилировать регулярное выражение.\n");
        return 1;
    }

    // Выполнение регулярного выражения
    result = regexec(&regex, text, 0, NULL, 0);
    
    // Проверка на совпадение
    if (!result) {
        printf("Совпадение найдено.\n");
    } else if (result == REG_NOMATCH) {
        printf("Совпадений нет.\n");
    } else {
        printf("Ошибка выполнения регулярного выражения.\n");
    }

    // Освобождение регулярного выражения
    regfree(&regex);

    return 0;
}
```
Пример вывода:
```
Совпадение найдено.
```

## Погружение в тему
Регулярные выражения используются с 1950-х годов, их распространение усилилось с появлением `ed` и `grep` в Unix. Альтернативы в C включают библиотеки функций для работы со строками и создание пользовательских парсеров, но regex более универсален. Под капотом `regex.h` реализует функциональность регулярных выражений, обычно через движки NFA (недетерминированный конечный автомат) или DFA (детерминированный конечный автомат).

## Смотрите также
- POSIX стандарт: https://pubs.opengroup.org/onlinepubs/9699919799/
- Учебник по регулярным выражениям (regex): https://www.regular-expressions.info/
- POSIX regex в C: http://man7.org/linux/man-pages/man3/regcomp.3.html
