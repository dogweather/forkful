---
title:                "Робота з CSV файлами"
html_title:           "Arduino: Робота з CSV файлами"
simple_title:         "Робота з CSV файлами"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/working-with-csv.md"
---

{{< edit_this_page >}}

## Що і чому?
Обробка CSV файлів - це читання та запис даних у вигляді тексту, розділеного комами. Програмісти цим займаються, бо так легко передавати дані між різними програмами.

## Як це робити:
```C
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {
    FILE *fp = fopen("data.csv", "r");
    if (!fp) {
        printf("Не можу відкрити файл\n");
        return 1;
    }

    char buf[1024];
    int row_count = 0;
    int field_count = 0;
    
    while (fgets(buf, 1024, fp)) {
        field_count = 0;
        row_count++;

        if (row_count == 1) {
            continue; // Пропустити заголовок
        }

        char *field = strtok(buf, ",");
        while (field) {
            if (field_count == 0) {
                printf("Ім'я: ");
            } else if (field_count == 1) {
                printf("Вік: ");
            } else if (field_count == 2) {
                printf("Місто: ");
            }

            printf("%s\n", field);
            field = strtok(NULL, ",");
            
            field_count++;
        }
        printf("\n");
    }

    fclose(fp);
    return 0;
}
```
**Вивід:**
```
Ім'я: Іван
Вік: 30
Місто: Київ

Ім'я: Оксана
Вік: 25
Місто: Львів
```

## Поглиблений розгляд
CSV - це абревіатура від "Comma-Separated Values", формат створений у 1972 році для простоти обробки табличних даних. Є альтернативи як JSON, XML, але CSV залишається популярним через свою простоту. Розробники мають вирішувати проблеми з екрануванням спеціальних символів, таких як коми всередині полів.

## Див. також
- [RFC 4180, стандарт CSV](https://tools.ietf.org/html/rfc4180)
- [Бібліотека libcsv для роботи з CSV у C](http://sourceforge.net/projects/libcsv/) 
- [Tutorial на cprogramming.com](https://www.cprogramming.com/tutorial/cfileio.html)