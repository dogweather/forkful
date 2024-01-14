---
title:                "C: Обчислення дати у майбутньому чи минулому"
programming_language: "C"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Чому

Розрахункова дата в майбутньому або в минулому може бути корисною для планування подій або визначення часу, який вже пройшов.

## Як це зробити

Найпростіший спосіб розрахувати дату в майбутньому або в минулому - це використовувати функцію `mktime` з бібліотеки `time.h`. Нижче показаний приклад коду для розрахунку дати + 10 днів:

```C
#include <stdio.h>
#include <time.h>

int main() {
    // отримання поточної дати та часу
    time_t t = time(NULL); 
    
    // розрахунок дати + 10 днів
    struct tm *future = localtime(&t);
    future->tm_mday += 10;
    t = mktime(future);

    // виведення результата
    printf("Дата через 10 днів: %s", asctime(localtime(&t)));
    
    return 0;
}
```

Вивід програми буде наступним:

```C
Дата через 10 днів: Sat Nov 13 13:15:26 2021
```

Щоб розрахувати дату в минулому, достатньо зменшити відповідне значення, наприклад `future->tm_mday -= 10;` для розрахунку дати - 10 днів.

## Розглиблення

У більш складних сценаріях, може бути потрібно розглянути різні аспекти, такі як різниця в часових зонах або перехід на літній/зимовий час. Також, важливо враховувати різні календарі, такі як григоріанський або юліанський.

Розрахунок дати в майбутньому або в минулому також можна виконати за допомогою комплексніших функцій, таких як `date_fromdm`, `date_difference` чи `date_additive`, доступних у різних бібліотеках для маніпулювання датами.

## Дивіться також

- [Функція mktime у бібліотеці time.h](https://www.cplusplus.com/reference/ctime/mktime/)
- [Функція localtime у бібліотеці time.h](https://www.cplusplus.com/reference/ctime/localtime/)
- [Комплексні операції з датами у бібліотеці date.h](https://manpages.debian.org/unstable/libdate-calc-perl/date.h.3.en.html)