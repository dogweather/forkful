---
title:                "Використання регулярних виразів"
date:                  2024-02-03T18:11:20.372528-07:00
model:                 gpt-4-0125-preview
simple_title:         "Використання регулярних виразів"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?

Регулярні вирази (regex) надають можливість шукати, відповідати і маніпулювати рядками, використовуючи визначені шаблони. Програмісти широко використовують їх для таких завдань, як перевірка введення, аналіз текстових даних та пошук шаблонів у великих текстових файлах, що робить їх потужним інструментом в будь-якій мові, включно з C.

## Як це робити:

Щоб використовувати регулярні вирази в C, ви головним чином будете працювати з бібліотекою POSIX regex (`<regex.h>`). Цей приклад демонструє базове відповідання шаблону:

```c
#include <stdio.h>
#include <stdlib.h>
#include <regex.h>

int main(){
    regex_t regex;
    int return_value;
    char *pattern = "^a[[:alnum:]]"; // Шаблон для пошуку рядків, що починаються з 'a', за якими слідують буквено-цифрові символи
    char *test_string = "apple123";

    // Компіляція регулярного виразу
    return_value = regcomp(&regex, pattern, REG_EXTENDED);
    if (return_value) {
        printf("Не вдалося скомпілювати regex\n");
        exit(1);
    }

    // Виконання регулярного виразу
    return_value = regexec(&regex, test_string, 0, NULL, 0);
    if (!return_value) {
        printf("Збіг знайдено\n");
    } else if (return_value == REG_NOMATCH) {
        printf("Збігів не знайдено\n");
    } else {
        printf("Помилка виконання regex\n");
        exit(1);
    }

    // Звільнення виділеної пам'яті, що використовувалась для regex
    regfree(&regex);

    return 0;
}
```

Приклад виводу для рядка, що відповідає ("apple123"):
```
Збіг знайдено
```
І для рядка, що не відповідає ("banana"):
```
Збігів не знайдено
```

## Поглиблений аналіз:

Регулярні вирази в C, як частина стандарту POSIX, пропонують надійний спосіб виконання відповідності та маніпуляцій з рядками. Проте API бібліотеки POSIX regex в C вважається більш обтяжливим, ніж ті, що знайдені в мовах, розроблених з функціями маніпуляції рядками першого класу, як-от Python або Perl. Синтаксис для шаблонів схожий між мовами, але в C потрібне ручне управління пам'яттю та більше шаблонного коду для підготовки, виконання та очищення після використання шаблонів regex.

Незважаючи на ці виклики, навчання використовувати regex в C є винагороджуваним, бо воно поглиблює розуміння концепцій програмування нижчого рівня. Крім того, це відкриває можливості для програмування на C у таких областях, як обробка тексту та екстракція даних, де regex є незамінним. Для більш складних шаблонів або операцій regex, альтернативи, такі як бібліотека PCRE (Perl Compatible Regular Expressions), можуть запропонувати більш багатий на функції та до певної міри легший інтерфейс, хоча це вимагатиме інтеграції зовнішньої бібліотеки у ваш проект на C.