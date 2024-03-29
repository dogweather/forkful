---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:54:12.877276-07:00
description: "\u041F\u043E\u0440\u0456\u0432\u043D\u044F\u043D\u043D\u044F \u0434\u0432\
  \u043E\u0445 \u0434\u0430\u0442 \u0432 C \u043F\u0435\u0440\u0435\u0434\u0431\u0430\
  \u0447\u0430\u0454 \u0432\u0438\u0437\u043D\u0430\u0447\u0435\u043D\u043D\u044F\
  \ \u0445\u0440\u043E\u043D\u043E\u043B\u043E\u0433\u0456\u0447\u043D\u043E\u0433\
  \u043E \u0432\u0456\u0434\u043D\u043E\u0448\u0435\u043D\u043D\u044F \u043C\u0456\
  \u0436 \u043D\u0438\u043C\u0438 \u2013 \u0447\u0438 \u043E\u0434\u043D\u0430 \u0434\
  \u0430\u0442\u0430 \u043F\u0435\u0440\u0435\u0434\u0443\u0454 \u0456\u043D\u0448\
  \u0456\u0439, \u0447\u0438 \u0432\u043E\u043D\u0438 \u043E\u0434\u043D\u0430\u043A\
  \u043E\u0432\u0456. \u0414\u0430\u043D\u0430 \u043C\u043E\u0436\u043B\u0438\u0432\
  \u0456\u0441\u0442\u044C \u0454\u2026"
lastmod: '2024-03-13T22:44:50.169846-06:00'
model: gpt-4-0125-preview
summary: "\u041F\u043E\u0440\u0456\u0432\u043D\u044F\u043D\u043D\u044F \u0434\u0432\
  \u043E\u0445 \u0434\u0430\u0442 \u0432 C \u043F\u0435\u0440\u0435\u0434\u0431\u0430\
  \u0447\u0430\u0454 \u0432\u0438\u0437\u043D\u0430\u0447\u0435\u043D\u043D\u044F\
  \ \u0445\u0440\u043E\u043D\u043E\u043B\u043E\u0433\u0456\u0447\u043D\u043E\u0433\
  \u043E \u0432\u0456\u0434\u043D\u043E\u0448\u0435\u043D\u043D\u044F \u043C\u0456\
  \u0436 \u043D\u0438\u043C\u0438 \u2013 \u0447\u0438 \u043E\u0434\u043D\u0430 \u0434\
  \u0430\u0442\u0430 \u043F\u0435\u0440\u0435\u0434\u0443\u0454 \u0456\u043D\u0448\
  \u0456\u0439, \u0447\u0438 \u0432\u043E\u043D\u0438 \u043E\u0434\u043D\u0430\u043A\
  \u043E\u0432\u0456. \u0414\u0430\u043D\u0430 \u043C\u043E\u0436\u043B\u0438\u0432\
  \u0456\u0441\u0442\u044C \u0454\u2026"
title: "\u041F\u043E\u0440\u0456\u0432\u043D\u044F\u043D\u043D\u044F \u0434\u0432\u043E\
  \u0445 \u0434\u0430\u0442"
---

{{< edit_this_page >}}

## Що і чому?

Порівняння двох дат в C передбачає визначення хронологічного відношення між ними – чи одна дата передує іншій, чи вони однакові. Дана можливість є важливою в додатках, що стосуються планування, строків завершення або ведення записів, оскільки вона дозволяє організовувати та маніпулювати часово-чутливими даними.

## Як це зробити:

В C немає вбудованого типу для дат, що змушує використовувати бібліотеку `time.h` для роботи зі структурами дат та часу. Структура `tm` та функція `difftime()` часто застосовуються для порівняння дат. Нижче наведено приклад, що показує, як порівняти дві дати:

```c
#include <stdio.h>
#include <time.h>

int main() {
    struct tm date1 = {0};
    struct tm date2 = {0};
    double seconds;

    // Перша дата (РРРР, ММ, ДД)
    date1.tm_year = 2023 - 1900; // Рік з 1900
    date1.tm_mon = 3 - 1;        // Місяць [0-11]
    date1.tm_mday = 15;          // День місяця [1-31]

    // Друга дата (РРРР, ММ, ДД)
    date2.tm_year = 2023 - 1900;
    date2.tm_mon = 4 - 1;
    date2.tm_mday = 14;

    // Перетворити в формат time_t
    time_t time1 = mktime(&date1);
    time_t time2 = mktime(&date2);

    // Порівняння
    seconds = difftime(time1, time2);

    if (seconds == 0) {
        printf("Дати однакові.\n");
    } else if (seconds > 0) {
        printf("Перша дата наступає після другої дати.\n");
    } else {
        printf("Перша дата наступає до другої дати.\n");
    }

    return 0;
}
```

Результат може бути:

```text
Перша дата наступає до другої дати.
```

Ця програма ініціалізує дві структури `tm` з конкретними датами, перетворює їх у формат `time_t` з використанням `mktime()`, та, нарешті, порівнює їх за допомогою `difftime()`, що повертає різницю в секундах (як `double`) між двома моментами часу.

## Поглиблений аналіз

На світанку C операції з датами та часом вимагали ручних розрахунків, часто з урахуванням високосних років, різної кількості днів у місяцях, а навіть високосних секунд. Впровадження `time.h` в стандарт ANSI C принесло стандартизацію обробки часу в C, спростивши операції з датами та часом.

Використання `time.h` для порівняння дат є простим, але має обмеження. Структура `tm` не враховує часові зони або літній час, а `difftime()` надає лише різницю у секундах, не маючи дрібнішої гранулярності для певних застосувань.

Для додатків, яким потрібні більш робастні операції з датою-часом, включно з підтримкою часових зон, переходів на літній час і більш точними інтервалами часу, бібліотеки, такі як `date.h` (бібліотека дат Говарда Хіннанта, що не є частиною стандартної бібліотеки) пропонують сучасну альтернативу `time.h`. Ці бібліотеки надають більш комплексні інструменти для маніпуляції з датою-часом у C++, скориставшись десятиліттями еволюції в дизайні мов програмування. Для програмістів C, використання цих зовнішніх бібліотек або ретельне врахування тонкощів обчислень дати-часу безпосередньо, залишається необхідним для досягнення точних та культурно обізнаних маніпуляцій з датою-часом.
