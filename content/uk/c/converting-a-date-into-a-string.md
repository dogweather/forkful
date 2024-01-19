---
title:                "Перетворення дати в рядок"
html_title:           "Lua: Перетворення дати в рядок"
simple_title:         "Перетворення дати в рядок"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Що це & Навіщо?
Перетворення дати в рядок - це процес, коли дату як об'єкт перетворюють в текстовий формат. Програмісти роблять це, щоб зручніше відображати дату та час, або зберігати їх у текстових файлах.

## Як це зробити:
Для перетворення дати в рядок в C ми використовуємо стандартну бібліотеку часу.

```C
#include <time.h>
#include <stdio.h>

int main() {
    time_t now;
    time(&now);
    char* date = ctime(&now);

    printf("Актуальна дата і час: %s\n", date);

    return 0;
}
```
В результаті ви отримаєте такий вивід:

```C
Актуальна дата і час: Wed Aug 28 10:34:28 2019
```

## Поглиблений огляд
Перетворення дати в рядок було важливою частиною C від самого початку. Відтоді багато бібліотек і мов додали свої власні варіанти цієї функції, але основний принцип залишився незмінним.

Один з альтернативних підходів - це використання `strftime()`, які надає більше контролю над форматом.

```C
#include <time.h>
#include <stdio.h>

int main() {
    char buffer[30];
    time_t now;
    struct tm* time_info;

    time(&now);
    time_info = localtime(&now);

    strftime(buffer, 30, "%d-%m-%Y %H:%M:%S", time_info);

    printf("Форматована дата і час: %s\n", buffer);

    return 0;
}
```

Зверніть увагу, що дана процедура використовує повну структуру часу `struct tm`, що допомагає краще керувати всіма аспектами дати і часу.

## Див. також 
Більше інформації про зазначену тематику можна знайти за наступними посиланнями:

1. [Стандартна бібліотека часу в C](http://www.cplusplus.com/reference/ctime/)
2. [Функція `ctime`](http://www.cplusplus.com/reference/ctime/ctime/)
3. [Функція `strftime`](http://www.cplusplus.com/reference/ctime/strftime/)