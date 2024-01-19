---
title:                "Використання регулярних виразів"
html_title:           "Arduino: Використання регулярних виразів"
simple_title:         "Використання регулярних виразів"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Що і чому?
Регулярні вирази - це могутній інструмент для роботи з рядками, що дозволяє знаходити, перевіряти або заміняти підрядки згідно з вказаним шаблоном. Програмісти використовують їх для ефективної обробки тексту та поверхової перевірки вводу.

## Як це робиться:
Отже, як ми можемо використовувати регулярні вирази в С? Використовуйте бібліотеку `<regex.h>`.

```C
#include <stdio.h>
#include <regex.h>

int main() {
    regex_t regex;
    int reti;

    reti = regcomp(&regex, "[а-яА-Я]*", 0);
    if (reti) {
        printf("Could not compile regex\n");
        return 1;
    }

    reti = regexec(&regex, "Привіт, Світ!", 0, NULL, 0);
    if (!reti) {
        puts("Match");
    } else if (reti == REG_NOMATCH) {
        puts("No match");
    } else {
        printf("Regex match failed\n");
        return 1;
    }

    regfree(&regex);
    return 0;
}
```
Якщо буде застосовано зазначений вище код, він перевірить, чи "Привіт, Світ!" є українським словом чи ні, і поверне "Match" або "No match".

## Зануримося глибше
Регулярні вирази виникли у 1956 році і є основою багатьох сучасних мов. Є також альтернативи, такі як парсери, для виконання більш складних завдань. 
Зверніть увагу, що регулярні вирази можуть бути дуже повільними, якщо ви використовуєте їх неправильно або великими масивами. 

## Дивись також
- POSIX regex documentation: http://pubs.opengroup.org/onlinepubs/009695399/basedefs/regex.h.html
- Stackoverflow How Do You Use Regular Expressions in C?: https://stackoverflow.com/questions/1085083/regular-expressions-in-c-examples
- Wikipedia article on Regular Expressions: https://uk.wikipedia.org/wiki/Регулярні_вирази