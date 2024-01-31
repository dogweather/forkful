---
title:                "Використання регулярних виразів"
date:                  2024-01-19
html_title:           "Bash: Використання регулярних виразів"
simple_title:         "Використання регулярних виразів"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Що це таке & навіщо?

Регулярні вирази - це потужний інструмент для пошуку та заміни тексту за певними патернами. Програмісти використовують їх для ефективної роботи з рядками: валідація даних, парсинг файлів, автоматизація текстових трансформацій.

## Як це зробити:

У C нема вбудованої підтримки регулярних виразів, але можна використати бібліотеку `<regex.h>`. Ось приклад простого пошуку:

```C
#include <stdio.h>
#include <regex.h>

int main() {
    regex_t regex;
    int ret;
    ret = regcomp(&regex, "^[a-z]+@[a-z]+\\.[a-z]+$", REG_EXTENDED);
    if (ret) {
        fprintf(stderr, "Could not compile regex\n");
        return 1;
    }
    
    // Матчимо email
    ret = regexec(&regex, "user@example.com", 0, NULL, 0);
    if (!ret) {
        puts("Valid email");
    } else if (ret == REG_NOMATCH) {
        puts("Invalid email");
    } else {
        regerror(ret, &regex, 0, 0);
        fprintf(stderr, "Regex match failed\n");
    }
    
    regfree(&regex);
    return 0;
}
```

Виведе: `Valid email`

## Під водою:

Регулярні вирази виникли у 1950-х, коли математик Стівен Кліни запропонував використання цих виразів для опису комплексних шаблонів у стрічках. У C підтримка регулярних виразів не вбудована на відміну від мов як Perl чи Python. Використовуючи `<regex.h>`, важливо звертати увагу на вивільнення пам'яті через `regfree`. Є альтернативні бібліотеки, як PCRE (Perl Compatible Regular Expressions), які пропонують більше можливостей.

## Також гляньте:

- POSIX regex MAN page: `man 7 regex`
- PCRE library: https://www.pcre.org/
- Online regex tester: https://regexr.com/
